unit uFtpListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFtpFileSource,
  uFileSource,
  uFileSourceOperationMessageBoxesUI;

type

  { TFtpListOperation }

  TFtpListOperation = class(TFileSourceListOperation)
  private
    FFtpFileSource: IFtpFileSource;
    FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;
    FFtpFileSourceConnection: TFtpFileSourceConnection;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, DCFileAttributes, uFileSourceOperationUI, uFile;

constructor TFtpListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FFtpFileSource := aFileSource as IFtpFileSource;

  inherited Create(aFileSource, aPath);

  FConnectionTimeout:= 1000;
  FFileSourceOperationMessageBoxesUI:= TFileSourceOperationMessageBoxesUI.Create;
  AddUserInterface(FFileSourceOperationMessageBoxesUI);
end;

destructor TFtpListOperation.Destroy;
begin
  FreeAndNil(FFileSourceOperationMessageBoxesUI);
  inherited Destroy;
end;

procedure TFtpListOperation.Initialize;
begin
  inherited Initialize;
  FFtpFileSourceConnection:= TFtpFileSourceConnection(FConnection);
end;

procedure TFtpListOperation.MainExecute;
var
  I : Integer;
  aFile: TFile;
  RemotePath: UTF8String;
begin
  FFiles.Clear;

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TFtpFileSource.CreateFile(Path);
    aFile.Name := '..';
    aFile.Attributes := S_IFDIR;
    FFiles.Add(aFile);
  end;

  if not Assigned(FConnection) then
  begin
    AskQuestion('Connection is busy. May be some operation already use it (coping, deleting etc.).', '',
                      [fsourOk], fsourOk, fsourOk);
    Exit;
  end;

  if FFtpFileSourceConnection.Connect(FFtpFileSource.URI) then

  with FFtpFileSourceConnection.FtpSend do
  begin
    RemotePath:= StringReplace(Path, PathDelim, '/', [rfReplaceAll]);
    if List(RemotePath, False) then
    for I:= 0 to FtpList.Count - 1 do
  begin
    if FtpList[I].FileName = '.' then Continue;

    aFile:= TFtpFileSource.CreateFile(Path, FtpList[I]);

    FFiles.Add(aFile);
  end;

  end;

end;

end.

