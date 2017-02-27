// *************************************************************************************************
// * ==> UTOptions --------------------------------------------------------------------------------*
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
 @abstract(@name contains the MD2 demo options form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTOptions;

interface

uses System.Classes,
     System.SysUtils,
     System.Variants,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ComCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages;

type
    {**
     MD2 demo options
    }
    TOptions = class(TForm)
        published
            laFPS: TLabel;
            ckFullScreen: TCheckBox;
            ckUseShader: TCheckBox;
            ckUsePreCalculatedLighting: TCheckBox;
            ckCollisions: TCheckBox;
            paFPS: TPanel;
            edFPS: TEdit;
            udFPS: TUpDown;
            btOk: TButton;

            procedure FormCreate(pSender: TObject);
            procedure edFPSExit(pSender: TObject);
            procedure btOkClick(pSender: TObject);
    end;

var
    Options: TOptions;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TOptions
//--------------------------------------------------------------------------------------------------
procedure TOptions.FormCreate(pSender: TObject);
begin
    // to see form in taskbar even if main form is still not created
    SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.edFPSExit(pSender: TObject);
begin
    // check if FPS is out of bounds
    if (StrToInt(edFPS.Text) > udFPS.Max) then
        edFPS.Text := IntToStr(udFPS.Max)
    else
    if (StrToInt(edFPS.Text) < udFPS.Min) then
        edFPS.Text := IntToStr(udFPS.Min);
end;
//--------------------------------------------------------------------------------------------------
procedure TOptions.btOkClick(pSender: TObject);
begin
    Close;
end;
//--------------------------------------------------------------------------------------------------

end.
