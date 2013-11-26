unit uNetworkCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uFileSource, uNetworkFileSource;

type

  TNetworkCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  private
    FNetworkFileSource: INetworkFileSource;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aDirectoryPath: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uFileSourceOperationUI, uLog, uLng, uGlobs;

constructor TNetworkCreateDirectoryOperation.Create(
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aDirectoryPath: String);
begin
  FNetworkFileSource := aTargetFileSource as INetworkFileSource;
  inherited Create(aTargetFileSource, aCurrentPath, aDirectoryPath);
end;

procedure TNetworkCreateDirectoryOperation.Initialize;
begin
end;

procedure TNetworkCreateDirectoryOperation.MainExecute;
begin
  if not Assigned(FConnection) then
  begin
    AskQuestion('Connection is busy. May be some operation already use it (coping, deleting etc.).', '',
                      [fsourOk], fsourOk, fsourOk);
    Exit;
  end;

  if FNetworkFileSource.CreateDirectory(FConnection, AbsolutePath) = False then
  begin
    if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogError + rsMsgLogMkDir, [AbsolutePath]), lmtError);

    AskQuestion(Format(rsMsgErrForceDir, [AbsolutePath]), '', [fsourOk], fsourOk, fsourOk);
  end
  else
  begin
    if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess + rsMsgLogMkDir, [AbsolutePath]), lmtSuccess);
  end;
end;

procedure TNetworkCreateDirectoryOperation.Finalize;
begin
end;

end.


