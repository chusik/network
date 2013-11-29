unit uNetworkFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceProperty, uFileSourceOperationTypes,
  uRealFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type

  INetworkFileSource = interface(IRealFileSource)
    ['{CDBDA3D4-B5CA-4092-9F8D-3D082268093E}']

    procedure FillAndCount(Connection: TObject; Files: TFiles;
      CountDirs: boolean; out NewFiles: TFiles;
      out FilesCount: int64; out FilesSize: int64);

    function DeleteFile(Connection: TObject; const aFile: UTF8String): boolean;

    function CreateDirectory(Connection: TObject; const aDirectory: UTF8String): boolean;
    function RemoveDirectory(Connection: TObject; const aDirectory: UTF8String): boolean;
  end;

  { TNetworkFileSource }

  TNetworkFileSource = class(TRealFileSource, INetworkFileSource)
  protected
    procedure FillAndCount(Connection: TObject; Files: TFiles;
      CountDirs: boolean; out NewFiles: TFiles;
      out FilesCount: int64; out FilesSize: int64); virtual; abstract;

    function DeleteFile(Connection: TObject; const aFile: UTF8String): boolean;
      virtual; abstract;

    function CreateDirectory(Connection: TObject; const aDirectory: UTF8String): boolean;
      virtual; abstract;
    function RemoveDirectory(Connection: TObject; const aDirectory: UTF8String): boolean;
      virtual; abstract;
  public
    function CreateCreateDirectoryOperation(BasePath: string;
      DirectoryPath: string): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
      override;
  end;

implementation

uses
  uNetworkCreateDirectoryOperation, uNetworkDeleteOperation, uNetworkCopyOutOperation;

{ TNetworkFileSource }

function TNetworkFileSource.CreateCreateDirectoryOperation(BasePath: string;
  DirectoryPath: string): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TNetworkCreateDirectoryOperation.Create(TargetFileSource,
    BasePath, DirectoryPath);
end;

function TNetworkFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TNetworkDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

end.
