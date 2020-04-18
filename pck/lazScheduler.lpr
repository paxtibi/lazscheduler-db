program lazScheduler;


{$DEFINE UseCThreads}

uses {$IFDEF UNIX}

     {$IFDEF UseCThreads}CThreads, {$ENDIF}
     {$ENDIF}
  scheduler.application,
  zcore,
  zdbc,
  zparsesql,
  zplain { add your units here };

begin
  Application.Title:='Scheduler';
  Application.Run;
  Application.Free;
end.

