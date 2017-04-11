// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

{**
 @abstract(@name is the main form for the resources installer tool.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Variants,
     System.UITypes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ImgList,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.ComCtrls,
     Vcl.Grids,
     Vcl.ValEdit,
     Winapi.Windows,
     Winapi.Messages,
     UTQRRADStudioHelper;

type
    {**
     Main form
    }
    TMainForm = class(TForm)
        published
            lvRADStudioVersions: TListView;
            ilIcons: TImageList;
            laInstalledVersions: TLabel;
            paDescription: TPanel;
            laPathsAndEnvironmentVariables: TLabel;
            veRADStudioVariables: TValueListEditor;
            btCopyResources: TButton;

            procedure FormCreate(pSender: TObject);
            procedure lvRADStudioVersionsChange(pSender: TObject; pItem: TListItem; change: TItemChange);
            procedure btCopyResourcesClick(pSender: TObject);

        public
            constructor Create(pOwner: TComponent); override;
            constructor CreateNew(pOwner: TComponent; dummy: Integer = 0); override;
            destructor Destroy; override;

            class function CopyResources(srcDir, destDir, resName: UnicodeString): Boolean; static;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Global resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// Global functions
//--------------------------------------------------------------------------------------------------
function EnumWindowsProc(hWnd: THandle; pProcessList: TStringList): Boolean; stdcall;
var
    caption: array [0..128] of Char;
begin
    SendMessage(hWnd, WM_GETTEXT, SizeOf(caption), NativeInt(@caption));
    pProcessList.AddObject(caption, TObject(hWnd));

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);
end;
//--------------------------------------------------------------------------------------------------
constructor TMainForm.CreateNew(pOwner: TComponent; dummy: Integer);
begin
    inherited CreateNew(pOwner, dummy);
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
var
    pVersions:           TQRRADStudioHelper.IQRInstalledVersions;
    version:             TQRRADStudioHelper.IInstalledVersionInfo;
    pPaths:              TQRRADStudioHelper.IQRKeyValueDictionary;
    pPathItem:           TQRRADStudioHelper.IQRKeyValuePair;
    selectedVersion:     EQRRadStudioVersion;
    pItem:               TListItem;
    param, filePath:     UnicodeString;
    i:                   Integer;
    succeeded, msgShown: Boolean;
begin
    pVersions := nil;

    try
        // get all installed Rad Studio versions on local computer
        pVersions := TQRRADStudioHelper.IQRInstalledVersions.Create;
        TQRRADStudioHelper.GetInstalledVersions(pVersions);

        // command line parameters?
        if (ParamCount > 0) then
        begin
            selectedVersion := EQRRadStudioVersion(-1);
            succeeded       := False;
            msgShown        := False;

            // iterate through command line parameters
            for i := 1 to ParamCount do
            begin
                // get parameter
                param := ParamStr(i);

                // is a valid parameter?
                if (param[1] <> '/') then
                    continue;

                // extract parameter value
                param := param.Substring(1, Length(param) - 1).ToLower;

                // search for wished Rad Studio version
                if (param = '4') then
                    selectedVersion := Delphi4
                else
                if (param = '5') then
                    selectedVersion := Delphi5
                else
                if (param = '6') then
                    selectedVersion := Delphi6
                else
                if (param = '7') then
                    selectedVersion := Delphi7
                else
                if (param = '8') then
                    selectedVersion := Delphi8
                else
                if (param = '2005') then
                    selectedVersion := Delphi2005
                else
                if (param = '2006') then
                    selectedVersion := Delphi2006
                else
                if (param = '2007') then
                    selectedVersion := Delphi2007
                else
                if (param = '2009') then
                    selectedVersion := Delphi2009
                else
                if (param = '2010') then
                    selectedVersion := Delphi2010
                else
                if (param = 'xe') then
                    selectedVersion := DelphiXE
                else
                if (param = 'xe2') then
                    selectedVersion := DelphiXE2
                else
                if (param = 'xe3') then
                    selectedVersion := DelphiXE3
                else
                if (param = 'xe4') then
                    selectedVersion := DelphiXE4
                else
                if (param = 'xe5') then
                    selectedVersion := DelphiXE5
                else
                if (param = 'xe6') then
                    selectedVersion := DelphiXE6
                else
                if (param = 'xe7') then
                    selectedVersion := DelphiXE7
                else
                if (param = 'xe8') then
                    selectedVersion := DelphiXE8
                else
                if (param = 'seattle') then
                    selectedVersion := Delphi10_Seattle
                else
                if (param = 'berlin') then
                    selectedVersion := Delphi10_1_Berlin
                else
                if (param = 'tokyo') then
                    selectedVersion := Delphi10_2_Tokyo
                else
                begin
                    MessageDlg('Unknown parameter - ' + param, mtError, [mbOK], 0);
                    continue;
                end;

                msgShown := False;

                // iterate through installed versions
                for version in pVersions do
                    // found selected version?
                    if (version.m_Version = selectedVersion) then
                    begin
                        // get Rad Studio installation path
                        filePath := ExtractFilePath(version.m_BinName);
                        pPaths   := nil;

                        try
                            // get paths and environment variables
                            pPaths := TQRRADStudioHelper.IQRKeyValueDictionary.Create;
                            TQRRADStudioHelper.GetPaths(filePath, pPaths);

                            // iterate through paths and environment variables
                            for pPathItem in pPaths do
                                // found Rad Studio common dir?
                                if (pPathItem.Key = 'BDSCOMMONDIR') then
                                    // copy the resources from local dir to common dir
                                    if (not CopyResources('..\',
                                                          IncludeTrailingPathDelimiter(pPathItem.Value) + 'Dcp\',
                                                          'UTQRVCLModelComponentGL.res'))
                                    then
                                    begin
                                        MessageDlg('Could not copy the UTQRVCLModelComponentGL.res resource file from:' +
                                                   #13#10                                                               +
                                                   ExtractFilePath(Application.ExeName)                                 +
                                                   #13#10                                                               +
                                                   'to:'                                                                +
                                                   #13#10                                                               +
                                                   IncludeTrailingPathDelimiter(pPathItem.Value) + 'Dcp\'               +
                                                   #13#10#13#10                                                         +
                                                   'You should copy this file manually.',
                                                   mtError,
                                                   [mbOK],
                                                   0);

                                        msgShown := True;
                                    end
                                    else
                                    begin
                                        succeeded := True;
                                        break;
                                    end;
                        finally
                            pPaths.Free;
                        end;
                    end;
            end;

            if (not succeeded and not msgShown) then
                MessageDlg('the UTQRVCLModelComponentGL.res resource file was not copied.' +
                           #13#10#13#10                                                    +
                           'You should copy this file manually.',
                           mtError,
                           [mbOK],
                           0);

            // all command line were processed, shutdown the application
            Application.ShowMainForm := False;
            Application.Terminate;
            Exit;
        end;

        for version in pVersions do
        begin
            // add a new item on the view and populate it
            pItem         := lvRADStudioVersions.Items.Add;
            pItem.Caption := g_RadStudioName[version.m_Version];
            pItem.SubItems.Add(version.m_BinName);

            // get RAD Studio icon from exe file
            TQRRADStudioHelper.ExtractIconFileToImageList(version.m_BinName, ilIcons);
            pItem.ImageIndex := ilIcons.Count - 1;
        end;
    finally
        pVersions.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.lvRADStudioVersionsChange(pSender: TObject; pItem: TListItem; change: TItemChange);
var
    fileName:  TFileName;
    filePath:  UnicodeString;
    pPaths:    TQRRADStudioHelper.IQRKeyValueDictionary;
    pPathItem: TQRRADStudioHelper.IQRKeyValuePair;
begin
    btCopyResources.Enabled := False;
    veRADStudioVariables.Strings.Clear;

    if (pItem.SubItems.Count <= 0) then
        Exit;

    fileName := pItem.SubItems.GetText;
    filePath := ExtractFilePath(fileName);
    pPaths   := nil;

    try
        pPaths := TQRRADStudioHelper.IQRKeyValueDictionary.Create;
        TQRRADStudioHelper.GetPaths(filePath, pPaths);

        for pPathItem in pPaths do
            veRADStudioVariables.InsertRow(pPathItem.Key, pPathItem.Value, True)
    finally
        pPaths.Free;
    end;

    btCopyResources.Enabled := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btCopyResourcesClick(pSender: TObject);
var
    row: Integer;
begin
    // get the common dir
    if (not veRADStudioVariables.FindRow('BDSCOMMONDIR', row)) then
    begin
        MessageDlg('The destination directory was not found in the Rad Studio environment variables.',
                   mtError,
                   [mbOK],
                   0);
        Exit;
    end;

    // copy the resources from local dir to common dir
    if (not CopyResources('',
                          IncludeTrailingPathDelimiter(veRADStudioVariables.Cells[1, row]) + 'Dcp\',
                          'UTQRVCLModelComponentGL.res'))
    then
        MessageDlg('Could not copy the UTQRVCLModelComponentGL.res resource file from:'      +
                   #13#10                                                                    +
                   ExtractFilePath(Application.ExeName)                                      +
                   #13#10                                                                    +
                   'to:'                                                                     +
                   #13#10                                                                    +
                   IncludeTrailingPathDelimiter(veRADStudioVariables.Cells[1, row]) + 'Dcp\' +
                   #13#10#13#10                                                              +
                   'You should copy this file manually.',
                   mtError,
                   [mbOK],
                   0)
    else
        MessageDlg('The resource file UTQRVCLModelComponentGL.res was copied successfully.',
                   mtInformation,
                   [mbOK],
                   0);
end;
//--------------------------------------------------------------------------------------------------
class function TMainForm.CopyResources(srcDir, destDir, resName: UnicodeString): Boolean;
var
    srcFileName, dstFileName: TFileName;
begin
    // get source file name
    if (Length(srcDir) > 0) then
        srcFileName := IncludeTrailingPathDelimiter(srcDir) + resName
    else
        srcFileName := resName;

    // source file name exists?
    if (not FileExists(srcFileName)) then
        Exit(False);

    // destination dir edists?
    if (not DirectoryExists(IncludeTrailingPathDelimiter(destDir))) then
        Exit(False);

    // get destination file name
    dstFileName := IncludeTrailingPathDelimiter(destDir) + resName;

    // copy file from source to destination
    Result := CopyFile(PWideChar(srcFileName), PWideChar(dstFileName), False);
end;
//--------------------------------------------------------------------------------------------------

end.
