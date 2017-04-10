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

uses System.SysUtils,
     System.Variants,
     System.Classes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ImgList,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Vcl.ComCtrls,
     Winapi.Windows,
     Winapi.Messages,
     UTQRRADStudioHelper, Vcl.Grids, Vcl.ValEdit;

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
            class function CopyResources(srcDir, destDir, resName: UnicodeString): Boolean; static;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Global resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
procedure TMainForm.FormCreate(pSender: TObject);
var
    pVersions: TQRRADStudioHelper.IQRInstalledVersions;
    version:   TQRRADStudioHelper.IInstalledVersionInfo;
    pItem:     TListItem;
begin
    pVersions := nil;

    try
        pVersions := TQRRADStudioHelper.IQRInstalledVersions.Create;

        TQRRADStudioHelper.GetInstalledVersions(pVersions);

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
