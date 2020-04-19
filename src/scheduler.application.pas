unit scheduler.application;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ModeSwitch typehelpers}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils, CustApp, ZDbcIntfs
  // , fgl
  ;


type

  TReschedulerTimeScale = (Undefined, TimeScaleYears, TimeScaleMonths, TimeScaleDays,
                           TimeScaleHours, TimeScaleMinutes, TimeScaleSeconds);


  TJobRecord = class
    id: uint64;
    command: string;
    running: boolean;
    scheduled_at: TDateTime;
    parameters: string;
    return_code: int64;
    pid: integer;
    outString: string;
    errString: string;
    resched_timescale: TReschedulerTimeScale;
    resched_quantity: integer;
  end;




  { TJob }
  TJob = class(TThread)
  private
    FExitStatus: integer;
    FonStart: TNotifyEvent;
    procedure SetExitStatus(AValue: integer);
    procedure SetonStart(AValue: TNotifyEvent);
  protected
    FConnection: IZConnection;
    FJob: TJobRecord;
  protected
    procedure setRunningProcess(j: TJobRecord);
    procedure setExitStatus(j: TJobRecord);
    procedure setReschedulateProcess(j: TJobRecord);
  public
    property Job: TJobRecord read FJob;
    property onStart: TNotifyEvent read FonStart write SetonStart;
  public
    constructor Create(aJob: TJobRecord);
    procedure Execute; override;
  end;

  { TSchedulerApplication }
  TSchedulerApplication = class(TCustomApplication)
  protected
    FConnection: IZConnection;
    FTriggerLevel: string;
    FParallelLevel: word;
    FExecuteAfterConnection: string;
  private
    FConnectionString: string;
    FMaxLoops: integer;
    FScanLoops: integer;
    FWaitSecondsAfter: integer;

    procedure loadJobsToRun;
    procedure bindDatabase;
  protected
    procedure DoRun; override;
    procedure onStart(Sender: TObject);
    procedure onTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ConnectionString: string read FConnectionString;

    property MaxLoops: integer read FMaxLoops write FMaxLoops;
    property WaitSecondsAfter: integer read FWaitSecondsAfter write FWaitSecondsAfter;
  end;

var
  Application: TSchedulerApplication;

implementation

uses
  process, dateutils, ZDbcMySql;

var
  FTablePrefix: string;
  FCS: TRTLCriticalSection;


function cast(Source: IZResultSet): TJobRecord;
var s: string;
begin
  Result := TJobRecord.Create;
  Result.id := Source.GetULongByName('id');
  Result.command := Source.GetUTF8StringByName('command');
  Result.running := Source.GetIntByName('running') <> 0;
  Result.scheduled_at := Source.GetDateByName('scheduled_at');
  Result.parameters := Source.GetUTF8StringByName('parameters');
  Result.return_code := Source.GetIntByName('return_code');


  s := Uppercase(Trim(Source.GetUTF8StringByName('resched_timescale')))+' ';
  case s[1] of
     'Y': Result.resched_timescale := TimeScaleYears;
     'M': Result.resched_timescale := TimeScaleMonths;
     'D': Result.resched_timescale := TimeScaleDays;
     'H': Result.resched_timescale := TimeScaleHours;
     'N': Result.resched_timescale := TimeScaleMinutes;
     'S': Result.resched_timescale := TimeScaleSeconds;
  else
     Result.resched_timescale := Undefined;
  end;

  Result.resched_quantity := Source.GetIntByName('resched_value');
end;

const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

{ TJob }

procedure TJob.SetExitStatus(AValue: integer);
begin
  if FExitStatus = AValue then
    Exit;
  FExitStatus := AValue;
end;

procedure TJob.SetonStart(AValue: TNotifyEvent);
begin
  if FonStart = AValue then
    Exit;
  FonStart := AValue;
end;

constructor TJob.Create(aJob: TJobRecord);
begin
  inherited Create(True);
  FJob := aJob;
  FreeOnTerminate := True;
end;

procedure TJob.setRunningProcess(j: TJobRecord);
var
  P: IZPreparedStatement;
begin
  EnterCriticalsection(FCS);
  P := FConnection.PrepareStatement(Format('update %sscheduler set running = 1 where id = ?', [FTablePrefix]));
  P.SetLong(1, j.id);
  P.ExecutePrepared;
  P := nil;
  P := FConnection.PrepareStatement(Format('insert %sscheduler_job(scheduler_id, pid) values(?,?)', [FTablePrefix]));
  P.SetLong(1, j.id);
  P.SetLong(2, j.pid);
  P.ExecutePrepared;
  P := nil;
  LeaveCriticalsection(FCS);
end;

procedure TJob.setExitStatus(j: TJobRecord);
var
  P: IZPreparedStatement;
begin
  EnterCriticalsection(FCS);
  FConnection.PingServer;
  P := FConnection.PrepareStatement(Format('insert %sscheduler_job_log(pid,log_level,message) values(?,0,?)', [FTablePrefix]));
  P.SetLong(1, j.pid);
  P.SetString(2, j.outString);
  P.ExecutePrepared;
  P := nil;

  P := FConnection.PrepareStatement(Format('insert %sscheduler_job_log(pid,log_level,message) values(?,1,?)', [FTablePrefix]));
  P.SetLong(1, j.pid);
  P.SetString(2, j.errString);
  P.ExecutePrepared;
  P := nil;

  P := FConnection.PrepareStatement(Format('update %sscheduler set running = 0, done = 1, return_code=? where id = ?', [FTablePrefix]));
  P.SetLong(1, j.return_code);
  P.SetLong(2, j.id);
  P.ExecutePrepared;
  P := nil;

  LeaveCriticalsection(FCS);
end;

procedure TJob.setReschedulateProcess(j: TJobRecord);
const ReschedSQL =
        'insert into oc_scheduler (                ' +
        '       `command`,    `running`,           ' +
        '       `done`,       `scheduled_at`,      ' +
        '       `parameters`, `created`,           ' +
        '       `update`,     `resched_timescale`, ' +
        '       `resched_value` )                  ' +
        'select `command`,    0,                   ' +
        '       0,            (`scheduled_at` + INTERVAL <VALUE> <TIMESCALE>),  ' +
        '       `parameters`, now(),               ' +
        '       now(),        `resched_timescale`, ' +
        '       `resched_value`                    ' +
        'from %sscheduler where id = ?             ';
var
   P: IZPreparedStatement;
   s: string;
begin
   EnterCriticalsection(FCS);
   try
     s:=Format(ReschedSQL, [FTablePrefix]);
     case j.resched_timescale of
        TimeScaleYears  : s:=StringReplace(s, '<TIMESCALE>', 'YEAR', []);
        TimeScaleMonths : s:=StringReplace(s, '<TIMESCALE>', 'MONTH', []);
        TimeScaleDays   : s:=StringReplace(s, '<TIMESCALE>', 'DAY', []);
        TimeScaleHours  : s:=StringReplace(s, '<TIMESCALE>', 'HOUR', []);
        TimeScaleMinutes: s:=StringReplace(s, '<TIMESCALE>', 'MINUTE', []);
        TimeScaleSeconds: s:=StringReplace(s, '<TIMESCALE>', 'SECOND', []);
     end;
     s:=StringReplace(s, '<VALUE>', j.resched_quantity.ToString, []);

     // writeln('SQL: ', s);
     FConnection.PingServer;
     P := FConnection.PrepareStatement(s);
     P.SetLong(1, j.id);
     try
        P.ExecutePrepared;
     except
     end;
   finally
     P := nil;
     LeaveCriticalsection(FCS);
   end;
end;

procedure TJob.Execute;
var
  P: TProcess;
  param: string;
  numbytes, bytesread, available: integer;
  outputlength, stderrlength: integer;
  stderrnumbytes, stderrbytesread: integer;
  outputstring: string;
  stderrstring: string;


    procedure _ReadStandardOutput;
    begin

      if (BytesRead + available > outputlength) then
      begin
        outputlength := BytesRead + READ_BYTES;
        Setlength(outputstring, outputlength);
      end;
      NumBytes := p.Output.Read(outputstring[1 + bytesread], available);
      if NumBytes > 0 then
        Inc(BytesRead, NumBytes);

    end;

    procedure _ReadStandardError;
    begin
      available := P.StdErr.NumBytesAvailable;
      if (StderrBytesRead + available > stderrlength) then
      begin
        stderrlength := StderrBytesRead + READ_BYTES;
        Setlength(stderrstring, stderrlength);
      end;
      StderrNumBytes := p.StdErr.Read(stderrstring[1 + StderrBytesRead], available);
      if StderrNumBytes > 0 then
        Inc(StderrBytesRead, StderrNumBytes);
    end;

begin
  FConnection := DriverManager.GetConnection(Application.connectionString);

  outputstring:='';
  stderrstring:='';

  try
    p := TProcess.Create(nil);
    try
      p.Executable := FJob.command;
      for param in FJob.parameters.split(' ') do
        p.Parameters.Add(param);
      p.Options := p.Options + [poUsePipes];
      bytesread := 0;
      outputlength := 0;
      stderrbytesread := 0;
      stderrlength := 0;
      try
         p.Execute;
         FJob.pid := P.ProcessID;
         setRunningProcess(FJob);
         if (FonStart <> nil) then
           FonStart(Self);
      except
        on e: exception do begin
           stderrstring:=e.Message;
           FJob.pid:=-1;
        end;
      end;

      while (FJob.pid>=0) and ( p.Running ) do
      begin
        available := P.Output.NumBytesAvailable;
        if available > 0 then
        begin
          _ReadStandardOutput;
        end
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
        begin
          _ReadStandardError;
        end
        else
          Sleep(100);
      end;
      available := P.Output.NumBytesAvailable;
      while available > 0 do
      begin
        _ReadStandardOutput;
        available := P.Output.NumBytesAvailable;
      end;
      setlength(outputstring, BytesRead);
      while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        _ReadStandardError;
      end;
      setlength(stderrstring, StderrBytesRead);
      FJob.return_code := p.exitstatus;
    except
      on e: exception do
         begin
           setlength(outputstring, BytesRead);
           raise;
         end;
    end;
  finally
    p.Free;
  end;
  FJob.errString := stderrstring;
  FJob.outString := outputstring;

  if OnTerminate <> nil then
  begin
    if FJob.resched_timescale <> Undefined then
       setReschedulateProcess(FJob);
    setExitStatus(FJob);
    OnTerminate(self);
  end;

  // free resources
  FConnection:=nil;
  FreeAndNil(FJob);
  Terminate;
  Free;
end;

{ TSchedulerApplication }

procedure TSchedulerApplication.DoRun;
begin

  if FMaxLoops > 0 then
     WriteLn('Loop ', FScanLoops);

  loadJobsToRun;
  inc(FScanLoops);

  if FMaxLoops > 0 then
     if FScanLoops>=FMaxLoops then begin
        writeln('closing connection...');
        FConnection.Close;
     end;

  sleep(1000 * WaitSecondsAfter);
  if FConnection.IsClosed then begin
     FConnection:=nil;
     terminate;
  end;
end;

procedure TSchedulerApplication.onStart(Sender: TObject);
begin
end;

procedure TSchedulerApplication.onTerminate(Sender: TObject);
begin
end;

constructor TSchedulerApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMaxLoops:=-1; // -1 = forever
  FScanLoops:=0;
  FWaitSecondsAfter:=30;

  InitCriticalSection(FCS);
  bindDatabase;
end;

destructor TSchedulerApplication.Destroy;
begin
  DoneCriticalsection(FCS);
  inherited Destroy;
end;

procedure TSchedulerApplication.loadJobsToRun;
var
  Rs: IZResultSet;
  P: IZPreparedStatement;
  J: TJob;
begin

  p := FConnection.PrepareStatement(Format('SELECT * FROM %sscheduler_next', [FTablePrefix]));
  rs := P.ExecuteQueryPrepared;

  while rs.Next do
  begin
    J := TJob.Create(cast(rs));
    J.onStart := @onStart;
    J.OnTerminate := @onTerminate;
    J.Start;
    sleep(100);
  end;

  rs := nil;
  p := nil;

end;

procedure TSchedulerApplication.bindDatabase;
var
  SL: TStringList;
  command: string;
begin
  if (FConnection <> nil) and (not FConnection.IsClosed) then
  begin
    FConnection.Close;
    FConnection := nil;
  end;
  SL := TStringList.Create;
  command := ChangeFileExt(ApplicationName, '.conf');
  SL.LoadFromFile(command);
  FConnectionString := Trim(sl.Values['dbc.url']);
  FTriggerLevel := Trim(sl.Values['logger.trigger']);
  FParallelLevel := 2;
  try
    FParallelLevel := word.Parse(Trim(sl.Values['parallel']));
  except
  end;

  FMaxLoops:=-1; // forever
  try
    FMaxLoops := Integer.Parse(Trim(sl.Values['maxloops']));
  except
  end;

  FWaitSecondsAfter:=30;
  try
    FWaitSecondsAfter := word.Parse(Trim(sl.Values['wait_seconds_after']));
  except
  end;

  FExecuteAfterConnection := sl.Values['execute-after-connection'];
  FTablePrefix := sl.Values['table-prefix'];
  FreeAndNil(SL);
  Log(etInfo, connectionString);
  FConnection := DriverManager.GetConnection(FConnectionString);
  Log(etInfo, 'Connectioned to db');
  for command in FExecuteAfterConnection.Split(';') do
  begin
    if command <> '' then
      FConnection.CreateStatement.Execute(command);
  end;
end;


initialization
  Application := TSchedulerApplication.Create(nil);


end.
