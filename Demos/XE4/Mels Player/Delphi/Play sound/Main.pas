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
 @abstract(@name contains the sound player demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.UITypes,
     System.Variants,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     UTQRPlayerAL,
     Winapi.Messages,
     Winapi.Windows;

type
    {**
     Main form
    }
    TMainForm = class(TForm)
        published
            paFile: TPanel;
            edFileName: TEdit;
            btSelect: TButton;
            btPlay: TButton;
            btPause: TButton;
            btStop: TButton;
            plPlayer: TQRPlayerAL;
            odOpen: TOpenDialog;
            tiTimer: TTimer;

            procedure FormCreate(pSender: TObject);
            procedure btPlayClick(pSender: TObject);
            procedure btPauseClick(pSender: TObject);
            procedure btStopClick(pSender: TObject);
            procedure btSelectClick(pSender: TObject);
            procedure tiTimerTimer(pSender: TObject);

        private
            m_Paused: Boolean;

        public
            {**
             Constructor
             @param(pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
{$R Main.res}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_Paused := False;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
var
    hPackageInstance: THandle;
    pStream:          TResourceStream;
    pSound:           TMemoryStream;
begin
    pStream := nil;
    pSound  := nil;

    try
        // get module instance at which this control belongs
        hPackageInstance := FindClassHInstance(TMainForm);

        // found module and package containing the texture JPEG image to load?
        if ((hPackageInstance <> 0) and
            (FindResource(hPackageInstance, PChar('ID_DEFAULT_SOUND'), RT_RCDATA) <> 0))
        then
        begin
            // load sound from resources
            pStream := TResourceStream.Create(hPackageInstance,
                                              PChar('ID_DEFAULT_SOUND'),
                                              RT_RCDATA);

            pSound := TMemoryStream.Create;
            pSound.CopyFrom(pStream, pStream.Size);
        end;

        // load sound
        if (not plPlayer.Open(pSound.Memory, pSound.Size)) then
        begin
            MessageDlg('Could not load sound.\r\n\r\nApplication will close.', mtError, [mbOK], 0);

            Application.Terminate;
            Exit;
        end;
    finally
        pSound.Free;
        pStream.Free
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btPlayClick(pSender: TObject);
begin
    btPause.Enabled := True;
    btStop.Enabled  := True;
    m_Paused        := False;

    plPlayer.Play;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btPauseClick(pSender: TObject);
begin
    if (plPlayer.IsPlaying) then
    begin
        m_Paused := True;
        plPlayer.Pause;
    end
    else
    begin
        m_Paused := False;
        plPlayer.Play;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btStopClick(pSender: TObject);
begin
    btPause.Enabled := False;
    btStop.Enabled  := False;
    m_Paused        := False;

    plPlayer.Stop;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btSelectClick(pSender: TObject);
var
    pFile:  TFileStream;
    pSound: TMemoryStream;
begin
    if (not odOpen.Execute) then
        Exit;

    if (ExtractFileExt(odOpen.FileName) <> '.wav') then
        Exit;

    pFile  := nil;
    pSound := nil;

    try
        // open sound file
        pFile  := TFileStream.Create(odOpen.FileName, fmOpenRead);
        pSound := TMemoryStream.Create;
        pSound.CopyFrom(pFile, pFile.Size);

        // load sound
        if (not plPlayer.Open(pSound.Memory, pSound.Size)) then
        begin
            MessageDlg('Could not load sound.', mtError, [mbOK], 0);
            Exit;
        end;

        edFileName.Text := odOpen.FileName;
    finally
        pSound.Free;
        pFile.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tiTimerTimer(pSender: TObject);
begin
    if ((not plPlayer.IsPlaying) and (not m_Paused)) then
    begin
        btPause.Enabled := False;
        btStop.Enabled  := False;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
