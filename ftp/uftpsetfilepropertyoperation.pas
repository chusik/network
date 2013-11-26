unit uFtpSetFilePropertyOperation;

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
  uFtpFileSource;

type

  { TFtpSetFilePropertyOperation }

  TFtpSetFilePropertyOperation = class(TNetworkSetFilePropertyOperation)
  private
    FFtpFileSource: IFtpFileSource;
    FFtpFileSourceConnection: TFtpFileSourceConnection;
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
  DCBasicTypes, DCStrUtils;

{ TFtpSetFilePropertyOperation }

constructor TFtpSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties);
begin
  FFtpFileSource := aTargetFileSource as IFtpFileSource;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName,
                           fpAttributes];
end;

procedure TFtpSetFilePropertyOperation.Initialize;
begin
  inherited Initialize;
  FFtpFileSourceConnection:= TFtpFileSourceConnection(FConnection);
end;

function TFtpSetFilePropertyOperation.SetNewProperty(aFile: TFile;
  aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  FileName: UTF8String;
  NewAttributes: String;
begin
  Result := sfprSuccess;

  with FFtpFileSourceConnection do
  case aTemplateProperty.GetID of
    fpName:
      begin
        if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
        begin
          FileName := StringReplace(aFile.FullPath, PathDelim, '/', [rfReplaceAll]);

          if not FtpSend.RenameFile(FileName, (aTemplateProperty as TFileNameProperty).Value) then
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
          NewAttributes := DecToOct((aTemplateProperty as TFileAttributesProperty).Value);
          FileName := StringReplace(aFile.FullPath, PathDelim, '/', [rfReplaceAll]);
          if (FtpSend.FTPCommand('SITE chmod' + #32 + NewAttributes + #32 + FileName) div 100) <> 2 then
            Result := sfprError;
        end
        else
          Result := sfprSkipped;
      end;

    fpModificationTime:
      begin
        Result := sfprSkipped;
      end;

    else
      raise Exception.Create('Trying to set unsupported property');
  end;
end;

end.

