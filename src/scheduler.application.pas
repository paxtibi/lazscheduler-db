unit scheduler.application;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ModeSwitch typehelpers}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils, CustApp, ZDbcIntfs, fgl;

type
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
  end;

  TJobRecordList = specialize TFPGList<TJobRecord>;

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
begin
  Result := TJobRecord.Create;
  Result.id := Source.GetULongByName('id');
  Result.command := Source.GetUTF8StringByName('command');
  Result.running := Source.GetIntByName('running') <> 0;
  Result.scheduled_at := Source.GetDateByName('scheduled_at');
  Result.parameters := Source.GetUTF8StringByName('parameters');
  Result.return_code := Source.GetIntByName('return_code');
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

procedure TJob.Execute;
var
  P: TProcess;
  param: string;
  numbytes, bytesread, available: integer;
  outputlength, stderrlength: integer;
  stderrnumbytes, stderrbytesread: integer;
  outputstring: string;
  stderrstring: string;
begin
  FConnection := DriverManager.GetConnection(Application.connectionString);

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
      p.Execute;
      FJob.pid := P.ProcessID;
      setRunningProcess(FJob);
      if (FonStart <> nil) then
        FonStart(Self);
      while p.Running do
      begin
        available := P.Output.NumBytesAvailable;
        if available > 0 then
        begin
          if (BytesRead + available > outputlength) then
          begin
            outputlength := BytesRead + READ_BYTES;
            Setlength(outputstring, outputlength);
          end;
          NumBytes := p.Output.Read(outputstring[1 + bytesread], available);
          if NumBytes > 0 then
            Inc(BytesRead, NumBytes);
        end
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
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
        end
        else
          Sleep(100);
      end;
      available := P.Output.NumBytesAvailable;
      while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
        begin
          outputlength := BytesRead + READ_BYTES;
          Setlength(outputstring, outputlength);
        end;
        NumBytes := p.Output.Read(outputstring[1 + bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available := P.Output.NumBytesAvailable;
      end;
      setlength(outputstring, BytesRead);
      while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available := P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
        begin
          stderrlength := StderrBytesRead + READ_BYTES;
          Setlength(stderrstring, stderrlength);
        end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1 + StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
      setlength(stderrstring, StderrBytesRead);
      FJob.return_code := p.exitstatus;
    except
      on e: Exception do
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
    OnTerminate(self);
    setExitStatus(FJob);
  end;
  FreeAndNil(FJob);
  Terminate;
end;

{ TSchedulerApplication }

procedure TSchedulerApplication.DoRun;
begin
  loadJobsToRun;
  sleep(1000 * 30);
  if FConnection.IsClosed then
    terminate;
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
  jr: TJobRecord;
  jl: TJobRecordList;
begin
  p := FConnection.PrepareStatement(Format('SELECT * FROM %sscheduler_next', [FTablePrefix]));
  rs := P.ExecuteQueryPrepared;
  jl := TJobRecordList.Create;
  while rs.Next do
  begin
    jr := cast(rs);
    jl.add(jr);
  end;
  for jr in jl do
  begin
    J := TJob.Create(jr);
    J.onStart := @onStart;
    J.OnTerminate := @onTerminate;
    J.Start;
    sleep(100);
  end;
  FreeAndNil(jl);
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
