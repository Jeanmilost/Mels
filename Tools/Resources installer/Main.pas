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
            laDescription: TLabel;

            procedure FormCreate(pSender: TObject);
            procedure lvRADStudioVersionsChange(pSender: TObject; pItem: TListItem; change: TItemChange);

        private

        public
    end;

var
  MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Global resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
var
    pItem:      TListItem;
    delphiComp: TDelphiVersions;
    fileName:   string;
    found:      Boolean;
begin
    // iterate through Delphi versions to detect
    for delphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
    begin
        // check if matching key exists in HKEY_CURRENT_USER
        found := TQRRADStudioHelper.RegKeyExists(g_DelphiRegPaths[delphiComp],
                                                 HKEY_CURRENT_USER);

        // found it?
        if (found) then
            // get the exe file name and check if the file exists
            found := TQRRADStudioHelper.RegReadStr(g_DelphiRegPaths[delphiComp],
                                                   'App',
                                                   fileName,
                                                   HKEY_CURRENT_USER) and FileExists(fileName);

        // still not found?
        if (not found) then
        begin
            // check if matching key exists in HKEY_LOCAL_MACHINE
            found := TQRRADStudioHelper.RegKeyExists(g_DelphiRegPaths[delphiComp],
                                                     HKEY_LOCAL_MACHINE);

            // found it?
            if (found) then
                // get the exe file name and check if the file exists
                found := TQRRADStudioHelper.RegReadStr(g_DelphiRegPaths[delphiComp],
                                                       'App',
                                                       fileName,
                                                       HKEY_LOCAL_MACHINE) and FileExists(fileName);
        end;

        // still not found?
        if (not found) then
            continue;

        // add a new item on the view and populate it
        pItem         := lvRADStudioVersions.Items.Add;
        pItem.Caption := g_DelphiVersionsNames[delphiComp];
        pItem.SubItems.Add(fileName);

        // get RAD Studio icon from exe file
        TQRRADStudioHelper.ExtractIconFileToImageList(ilIcons, fileName);
        pItem.ImageIndex := ilIcons.Count - 1;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.lvRADStudioVersionsChange(pSender: TObject; pItem: TListItem; change: TItemChange);
begin
    // see: C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat
end;
//--------------------------------------------------------------------------------------------------

end.
