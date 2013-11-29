unit uSftpSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uNetworkSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileProperty,
  uNetworkFileSource,
  uSftpFileSource;

type

  { TSftpSetFilePropertyOperation }

  TSftpSetFilePropertyOperation = class(TNetworkSetFilePropertyOperation)
  private
    FSftpFileSource: ISftpFileSource;
    FSftpFileSourceConnection: TSftpFileSourceConnection;
  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    procedure Initialize; override;
  end;

implementation

uses
  DCBasicTypes, DCStrUtils, DCDateTimeUtils,  uNetworkFileSourceUtil,  libssh;

{ TSftpSetFilePropertyOperation }

constructor TSftpSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties);
begin
  FSftpFileSource := aTargetFileSource as ISftpFileSource;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName,
                           fpLastAccessTime,
                           fpModificationTime,
                           fpAttributes];
end;

procedure TSftpSetFilePropertyOperation.Initialize;
begin
  inherited Initialize;
  FSftpFileSourceConnection:= TSftpFileSourceConnection(FConnection);
end;

function TSftpSetFilePropertyOperation.SetNewProperty(aFile: TFile;
  aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  FileName,
  NewFileName: UTF8String;
  NewAttributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Result := sfprSuccess;

  with FSftpFileSourceConnection do
  case aTemplateProperty.GetID of
    fpName:
      begin
        if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
        begin
          FileName := CreateNetworkPath(aFile.FullPath);
          NewFileName:= CreateNetworkPath(aFile.Path + (aTemplateProperty as TFileNameProperty).Value);
          if libssh2_sftp_rename(Session_, PAnsiChar(FileName), PAnsiChar(NewFileName)) < 0 then
            Result := sfprError;
        end
        else
          Result := sfprSkipped;
      end;

    fpAttributes:
      begin
        if (aTemplateProperty as TFileAttributesProperty).Value <>
           (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
        begin
          FileName := CreateNetworkPath(aFile.FullPath);
          NewAttributes.flags:= LIBSSH2_SFTP_ATTR_PERMISSIONS;
          NewAttributes.permissions:= (aTemplateProperty as TFileAttributesProperty).Value;
          if libssh2_sftp_setstat(Session_, PAnsiChar(FileName), @NewAttributes) < 0 then
            Result := sfprError;
        end
        else
          Result := sfprSkipped;
      end;

    fpModificationTime:
      begin
         if (aTemplateProperty as TFileModificationDateTimeProperty).Value <>
            (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value then
         begin
           NewAttributes.flags:= LIBSSH2_SFTP_ATTR_ACMODTIME;
           NewAttributes.atime:= DateTimeToUnixFileTime(aFile.LastAccessTime);
           NewAttributes.mtime:= DateTimeToUnixFileTime((aTemplateProperty as TFileModificationDateTimeProperty).Value);
           if libssh2_sftp_setstat(Session_, PAnsiChar(FileName), @NewAttributes) < 0 then
             Result := sfprError;
         end
         else
           Result := sfprSkipped;
      end;

     fpLastAccessTime:
       begin
         if (aTemplateProperty as TFileLastAccessDateTimeProperty).Value <>
            (aFile.Properties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value then
         begin
           NewAttributes.flags:= LIBSSH2_SFTP_ATTR_ACMODTIME;
           NewAttributes.mtime:= DateTimeToUnixFileTime(aFile.ModificationTime);
           NewAttributes.atime:= DateTimeToUnixFileTime((aTemplateProperty as TFileLastAccessDateTimeProperty).Value);
           if libssh2_sftp_setstat(Session_, PAnsiChar(FileName), @NewAttributes) < 0 then
             Result := sfprError;
         end
         else
           Result := sfprSkipped;
       end

    else
      raise Exception.Create('Trying to set unsupported property');
  end;
end;

end.

