unit uSftpFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser, blcksock, libssh,
  uFileSourceProperty, uFileSourceOperationTypes,
  uNetworkFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type

  ISftpFileSource = interface(INetworkFileSource)
    ['{87D0A3EF-C168-44C1-8B10-3AEC0753846A}']
  end;

  { TSftpFileSource }

  TSftpFileSource = class(TNetworkFileSource, ISftpFileSource)
  private
    FMainConnection: TFileSourceConnection;
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
  protected
    procedure FillAndCount(Connection: TObject; Files: TFiles; CountDirs: Boolean;
                           out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64); override;
    function DeleteFile(Connection: TObject; const aFile: UTF8String): Boolean; override;
    function CreateDirectory(Connection: TObject; const aDirectory: UTF8String): Boolean; override;
    function RemoveDirectory(Connection: TObject; const aDirectory: UTF8String): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function IsSupportedPath(const Path: String): Boolean; override;

    class function CreateFile(const APath: String): TFile; override;
    class function CreateFile(const APath, AName: String; Attrs: PLIBSSH2_SFTP_ATTRIBUTES): TFile;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; override;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

  { TSftpFileSourceConnection }

  TSftpFileSourceConnection = class(TFileSourceConnection)
  private
    sock:TTCPBlockSocket;
    session:PLIBSSH2_SESSION;
    sftp_session: PLIBSSH2_SFTP;
    procedure FTPStatus(Sender: TObject; Response: Boolean; const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Connect(const URI: TURI): Boolean;
    property Session_: PLIBSSH2_SFTP read sftp_session;
    property ss: PLIBSSH2_SESSION read session;
  end;

implementation

uses
  LCLProc, DCDateTimeUtils, uSftpListOperation, uSftpCopyOutOperation, uLog,
  uShowMsg, uNetworkFileSourceUtil, uSftpSetFilePropertyOperation;

{ TSftpFileSourceConnection }

procedure TSftpFileSourceConnection.FTPStatus(Sender: TObject;
  Response: Boolean; const Value: string);
begin
  if Response then
    logWrite(Value, lmtSuccess, True)
  else if Pos('PASS ', Value) > 0 then
    logWrite('PASS *********', lmtInfo, True)
  else
    logWrite(Value, lmtInfo, True);
end;

constructor TSftpFileSourceConnection.Create;
begin
  inherited Create;
  sock := TTCPBlockSocket.Create;
end;

destructor TSftpFileSourceConnection.Destroy;
begin
      libssh2_sftp_shutdown(sftp_session);
  libssh2_session_disconnect(session,'Thank you for using sshtest');
     libssh2_session_free(session);
     sock.Free;
  inherited Destroy;
end;

function TSftpFileSourceConnection.Connect(const URI: TURI): Boolean;
const
  HOSTKEY_SIZE = 20;
var
  I: Integer;
  S: String;
  FingerPrint: array [0..Pred(HOSTKEY_SIZE)] of AnsiChar;
begin
  logWrite('Connecting to: ' + URI.Host, lmtInfo, True);
  sock.Connect(PAnsiChar('192.168.56.102'),'22');
  if sock.LastError=0 then
    begin
    session := libssh2_session_init();
    if libssh2_session_startup(session, sock.Socket)<>0 then
    begin
      logWrite('Cannot establishing SSH session', lmtError, True);
      Exit(False);
    end;
    logWrite('Connection established', lmtSuccess, True);
    FingerPrint := libssh2_hostkey_hash(session, LIBSSH2_HOSTKEY_HASH_SHA1);
    S:= 'Server fingerprint:';
    i:=0;
    for I:= Low(FingerPrint) to High(FingerPrint) do
    begin
      S:= S + #32 + IntToHex(Ord(FingerPrint[i]), 2);
      //Inc(I);
    end;
    logWrite(S, lmtInfo, True);
    if libssh2_userauth_password(session, pchar('alexx'), pchar('198539'))<>0 then
    begin
      logWrite('Authentication by password failed', lmtError, True);
      Exit(False);
    end;
    logWrite('Authentication succeeded', lmtSuccess, True);
    sftp_session := libssh2_sftp_init(session);
    //* Since we have not set non-blocking, tell libssh2 we are blocking */
    libssh2_session_set_blocking(session, 1);
    end;
    Result:= Assigned(sftp_session);
end;

constructor TSftpFileSource.Create;
begin
  inherited Create;
  FMainConnection:= TSftpFileSourceConnection.Create;
end;

destructor TSftpFileSource.Destroy;
begin
  FreeAndNil(FMainConnection);
  inherited Destroy;
end;

class function TSftpFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= (Pos('sftp://', Path) = 1);
end;

class function TSftpFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty:= TFileSizeProperty.Create;
    LastAccessTimeProperty:= TFileLastAccessDateTimeProperty.Create;
    ModificationTimeProperty:= TFileModificationDateTimeProperty.Create;
    AttributesProperty := TUnixFileAttributesProperty.Create;
  end;
end;

class function TSftpFileSource.CreateFile(const APath, AName: String;
  Attrs: PLIBSSH2_SFTP_ATTRIBUTES): TFile;
begin
  Result:= CreateFile(APath);

  Result.Name:= AName;
  Result.Attributes:= Attrs^.permissions;
  if not Result.IsDirectory then Result.Size:= Attrs^.filesize;
  Result.LastAccessTime:= UnixFileTimeToDateTime(Attrs^.atime);
  Result.ModificationTime:= UnixFileTimeToDateTime(Attrs^.mtime);
end;

function TSftpFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  if not Operation.WantsNewConnection then
    Result:= FMainConnection
  else
    Result:= TSftpFileSourceConnection.Create;
end;

function TSftpFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCreateDirectory, fsoDelete, fsoCopyOut, fsoSetFileProperty];
end;

function TSftpFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspUsesConnections, fspListInMainThread];
end;

function TSftpFileSource.GetFreeSpace(Path: String; out FreeSize,
  TotalSize: Int64): Boolean;
var
  Ret: Integer;
  RemotePath: UTF8String;
  Stat: _LIBSSH2_SFTP_STATVFS;
begin
  RemotePath:= CreateNetworkPath(Path);
  repeat
    Ret:= libssh2_sftp_statvfs((FMainConnection as TSftpFileSourceConnection).Session_,
                               PAnsiChar(RemotePath), Length(RemotePath), @Stat);
  until Ret <> LIBSSH2_ERROR_EAGAIN;
  Result:= (Ret = 0);
  if Result then
  begin
    FreeSize:= (Int64(Stat.f_bavail) * Stat.f_bsize);
    TotalSize:= (Int64(Stat.f_blocks) * Stat.f_bsize);
  end;
end;

function TSftpFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties +
            [fpSize, fpLastAccessTime, fpModificationTime, fpAttributes];
end;

procedure TSftpFileSource.FillAndCount(Connection: TObject; Files: TFiles;
  CountDirs: Boolean; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
var
  I: Integer;
  aFile: TFile;
  aConnection: TSftpFileSourceConnection absolute Connection;

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    Return: Integer;
    RemotePath: UTF8String;
    Handle: PLIBSSH2_SFTP_HANDLE;
    aFileName: array[0..1023] of AnsiChar;
    aFullData: array[0..1023] of AnsiChar;
    Attributes: LIBSSH2_SFTP_ATTRIBUTES;
  begin
    RemotePath:= CreateNetworkPath(srcPath);
        //* Request a dir listing via SFTP */
    Handle := libssh2_sftp_opendir(aConnection.Session_, PAnsiChar(RemotePath));
    if (Handle = nil) then Exit;

    repeat        //* loop until we fail */
      Return:= libssh2_sftp_readdir_ex(Handle, aFileName, SizeOf(aFileName),
                                       aFullData, SizeOf(aFullData), @Attributes);
      if (Return > 0) then
      begin
        if (aFileName = '.') or (aFileName = '..') then Continue;

        aFile:= TSftpFileSource.CreateFile(srcPath, aFileName, @Attributes);

        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            if CountDirs then Inc(FilesCount);
            FillAndCountRec(srcPath + aFileName + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;
      end;
    until (Return <= 0);
    libssh2_sftp_closedir(Handle);
  end;

begin
  FilesCount:= 0;
  FilesSize:= 0;

  NewFiles := TFiles.Create(Files.Path);
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I];

    NewFiles.Add(aFile.Clone);

    if aFile.AttributesProperty.IsDirectory {and (not aFile.LinkProperty.IsLinkToDirectory)} then
      begin
        if CountDirs then
          Inc(FilesCount);
        FillAndCountRec(aFile.FullPath + DirectorySeparator);  // recursive browse child dir
      end
    else
      begin
        Inc(FilesCount);
        Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
      end;
  end;
end;

function TSftpFileSource.DeleteFile(Connection: TObject; const aFile: UTF8String): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TSftpFileSourceConnection absolute Connection;
begin
  aRemotePath:= CreateNetworkPath(aFile);
  Result:= libssh2_sftp_unlink(aConnection.sftp_session, PAnsiChar(aRemotePath)) = 0;
end;

function TSftpFileSource.CreateDirectory(Connection: TObject;
  const aDirectory: UTF8String): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TSftpFileSourceConnection absolute Connection;
begin
  aRemotePath:= CreateNetworkPath(aDirectory);
  Result:= libssh2_sftp_mkdir(aConnection.sftp_session,
                              PAnsiChar(aRemotePath),
                              LIBSSH2_SFTP_S_IRWXU or
                              LIBSSH2_SFTP_S_IRGRP or LIBSSH2_SFTP_S_IXGRP or
                              LIBSSH2_SFTP_S_IROTH or LIBSSH2_SFTP_S_IXOTH) = 0;
end;

function TSftpFileSource.RemoveDirectory(Connection: TObject;
  const aDirectory: UTF8String): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TSftpFileSourceConnection absolute Connection;
begin
  aRemotePath:= CreateNetworkPath(aDirectory);
  Result:= libssh2_sftp_rmdir(aConnection.sftp_session, PAnsiChar(aRemotePath)) = 0;
end;

function TSftpFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource:= Self;
  Result:= TSftpListOperation.Create(TargetFileSource, TargetPath);
end;

function TSftpFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;

end;

function TSftpFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TSftpCopyOutOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TSftpFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TSftpSetFilePropertyOperation.Create(TargetFileSource, theTargetFiles, theNewProperties);
end;

end.
