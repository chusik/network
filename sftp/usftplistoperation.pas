unit uSftpListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uSftpFileSource,
  uFileSource,
  uFileSourceOperationMessageBoxesUI;

type

  { TSftpListOperation }

  TSftpListOperation = class(TFileSourceListOperation)
  private
    FSftpFileSource: ISftpFileSource;
    FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;
    FCurlFileSourceConnection: TSftpFileSourceConnection;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, DCFileAttributes, uFileSourceOperationUI, uFile, libssh,
  uNetworkFileSourceUtil;

constructor TSftpListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FSftpFileSource := aFileSource as ISftpFileSource;

  inherited Create(aFileSource, aPath);

  FConnectionTimeout:= 1000;
  FFileSourceOperationMessageBoxesUI:= TFileSourceOperationMessageBoxesUI.Create;
  AddUserInterface(FFileSourceOperationMessageBoxesUI);
end;

destructor TSftpListOperation.Destroy;
begin
  FreeAndNil(FFileSourceOperationMessageBoxesUI);
  inherited Destroy;
end;

procedure TSftpListOperation.Initialize;
begin
  inherited Initialize;
  FCurlFileSourceConnection:= TSftpFileSourceConnection(FConnection);
end;

procedure TSftpListOperation.MainExecute;
var
  aFile: TFile;
  Return: Integer;
  Handle: PLIBSSH2_SFTP_HANDLE;
  aFileName: array[0..1023] of AnsiChar;
  aFullData: array[0..1023] of AnsiChar;
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
  RemotePath: UTF8String;
begin
  FFiles.Clear;

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TSftpFileSource.CreateFile(Path);
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

  if FCurlFileSourceConnection.Connect(FSftpFileSource.URI) then

  with FCurlFileSourceConnection do
  begin
    RemotePath:= CreateNetworkPath(Path);
        //* Request a dir listing via SFTP */
    Handle := libssh2_sftp_opendir(Session_, PAnsiChar(RemotePath));

    repeat        //* loop until we fail */
      Return:= libssh2_sftp_readdir_ex(Handle, aFileName, SizeOf(aFileName),
                                       aFullData, SizeOf(aFullData), @Attributes);
      if (Return > 0) then
      begin
        WriteLn(aFullData);
        WriteLn(aFileName);

        if (aFileName = '.') or (aFileName = '..') then Continue;

        aFile:= TSftpFileSource.CreateFile(FConnection, Path, aFileName, @Attributes);

        FFiles.Add(aFile);
      end;
    until (Return <= 0);
    libssh2_sftp_closedir(Handle);
  end;

end;

end.

