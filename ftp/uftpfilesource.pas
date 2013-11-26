unit uFtpFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser, FtpSend,
  uFileSourceProperty, uFileSourceOperationTypes,
  uNetworkFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type

  IFtpFileSource = interface(INetworkFileSource)
    ['{87D0A3EF-C168-44C1-8B10-3AEC0753846A}']
  end;

  { TFtpFileSource }

  TFtpFileSource = class(TNetworkFileSource, IFtpFileSource)
  private
    FMainConnection: TFileSourceConnection;
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
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
    class function CreateFile(const APath: String; AFTPListRec: TFTPListRec): TFile;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; override;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

  { TFtpFileSourceConnection }

  TFtpFileSourceConnection = class(TFileSourceConnection)
  private
    FFtpSend: TFTPSend;
    procedure FTPStatus(Sender: TObject; Response: Boolean; const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Connect(const URI: TURI): Boolean;
    property FtpSend: TFTPSend read FFtpSend;
  end;

implementation

uses
  LCLProc, uFtpListOperation, uLog, uShowMsg, uFtpFileSourceUtil,
  DCFileAttributes, uFtpSetFilePropertyOperation;

{ TFtpFileSourceConnection }

procedure TFtpFileSourceConnection.FTPStatus(Sender: TObject;
  Response: Boolean; const Value: string);
begin
  if Response then
    logWrite(Value, lmtSuccess, True)
  else if Pos('PASS ', Value) > 0 then
    logWrite('PASS *********', lmtInfo, True)
  else
    logWrite(Value, lmtInfo, True);
end;

constructor TFtpFileSourceConnection.Create;
begin
  inherited Create;
  FFtpSend:= TFTPSend.Create;
  FFtpSend.OnStatus:= @FTPStatus;
end;

destructor TFtpFileSourceConnection.Destroy;
begin
  FFtpSend.Free;
  inherited Destroy;
end;

function TFtpFileSourceConnection.Connect(const URI: TURI): Boolean;
begin
  FFtpSend.TargetHost:= URI.Host;
  if Length(URI.Username) > 0 then FFtpSend.UserName:= URI.Username;
  if Length(URI.Password) > 0 then FFtpSend.Password:= URI.Password;
  if URI.Port > 0 then FFtpSend.TargetPort:= IntToStr(URI.Port);

  Result:= FFtpSend.Login;
  if Result then
  begin
        FFtpSend.FTPCommand('FEAT');
  end;
end;

constructor TFtpFileSource.Create;
begin
  inherited Create;
  FMainConnection:= TFtpFileSourceConnection.Create;
end;

destructor TFtpFileSource.Destroy;
begin
  FreeAndNil(FMainConnection);
  inherited Destroy;
end;

class function TFtpFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= (Pos('ftp://', Path) = 1) or (Pos('ftps://', Path) = 1);
end;

class function TFtpFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty:= TFileSizeProperty.Create;
    ModificationTimeProperty:= TFileModificationDateTimeProperty.Create;
    AttributesProperty := TUnixFileAttributesProperty.Create;
  end;
end;

class function TFtpFileSource.CreateFile(const APath: String;
  AFTPListRec: TFTPListRec): TFile;
begin
  Result:= CreateFile(APath);

  Result.Attributes:= UnixStrToFileAttr(AFTPListRec.Permission);
  Result.Name:= AFTPListRec.FileName;
  Result.ModificationTime:= AFTPListRec.FileTime;
  if not Result.IsDirectory then Result.Size:= AFTPListRec.FileSize;
end;

function TFtpFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  if not Operation.WantsNewConnection then
    Result:= FMainConnection
  else
    Result:= TFtpFileSourceConnection.Create;
end;

function TFtpFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCreateDirectory, fsoDelete, fsoSetFileProperty];
end;

function TFtpFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual, fspUsesConnections, fspListInMainThread];
end;

function TFtpFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties +
            [fpSize, fpModificationTime, fpAttributes];
end;

procedure TFtpFileSource.FillAndCount(Connection: TObject; Files: TFiles;
  CountDirs: Boolean; out NewFiles: TFiles; out FilesCount: Int64; out
  FilesSize: Int64);
var
  I: Integer;
  aFile: TFile;
  aConnection: TFtpFileSourceConnection absolute Connection;

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    J, Return: Integer;
    RemotePath: UTF8String;
    aFileList: TFTPListEx;
  begin
    RemotePath:= StringReplace(srcPath, PathDelim, '/', [rfReplaceAll]);
        //* Request a dir listing via SFTP */
    if not aConnection.FtpSend.List(RemotePath, False) then Exit;
    aFileList:= TFTPListEx.Create;
    aFileList.Assign(aConnection.FtpSend.FtpList);
    for J:= 0 to aFileList.Count - 1 do
    begin//* loop until we fail */

        if (aFileList[J].FileName = '.') or (aFileList[J].FileName = '..') then Continue;

        aFile:= TFtpFileSource.CreateFile(srcPath, aFileList[J]);

        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            if CountDirs then Inc(FilesCount);
            FillAndCountRec(srcPath + aFileList[J].FileName + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;
    end;
    aFileList.Free;
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

function TFtpFileSource.DeleteFile(Connection: TObject; const aFile: UTF8String
  ): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TFtpFileSourceConnection absolute Connection;
begin
  aRemotePath:= StringReplace(aFile, PathDelim, '/', [rfReplaceAll]);
  Result:= aConnection.FtpSend.DeleteFile(aRemotePath);
end;

function TFtpFileSource.CreateDirectory(Connection: TObject;
  const aDirectory: UTF8String): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TFtpFileSourceConnection absolute Connection;
begin
  aRemotePath:= StringReplace(aDirectory, PathDelim, '/', [rfReplaceAll]);
  Result:= aConnection.FtpSend.CreateDir(aRemotePath);
end;

function TFtpFileSource.RemoveDirectory(Connection: TObject;
  const aDirectory: UTF8String): Boolean;
var
  aRemotePath: UTF8String;
  aConnection: TFtpFileSourceConnection absolute Connection;
begin
  aRemotePath:= StringReplace(aDirectory, PathDelim, '/', [rfReplaceAll]);
  Result:= aConnection.FtpSend.DeleteDir(aRemotePath);
end;

function TFtpFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource:= Self;
  //TargetPath:= StringReplace(TargetPath, PathDelim, '/', [rfReplaceAll]);
  Result:= TFtpListOperation.Create(TargetFileSource, TargetPath);
end;

function TFtpFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;

end;

function TFtpFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFtpSetFilePropertyOperation.Create(TargetFileSource, theTargetFiles, theNewProperties);
end;

end.
