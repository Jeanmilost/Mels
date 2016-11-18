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
 @abstract(@name contains the MD3 demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     System.Variants,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ExtCtrls,
     Vcl.ComCtrls,
     Vcl.Forms,
     Vcl.Menus,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages,
     UTQR3D,
     UTQRGeometry,
     UTQRCollision,
     UTOptions,
     {$IF CompilerVersion <= 25}
         // for compiler until XE4 (not sure until which version), the DelphiGL library is required,
         // because the OpenGL include provided by Embarcadero is incomplete
         XE7.OpenGL,
         XE7.OpenGLext;
     {$ELSE}
         Winapi.OpenGL,
         Winapi.OpenGLext;
     {$ENDIF}

type
    {**
     MD2 demo main form
    }
    TMainForm = class(TForm)
        published
            pbLoadModel: TProgressBar;
            paRendering: TPanel;
            pmOptions: TPopupMenu;
            miPrevTorsoAnim: TMenuItem;
            miNextTorsoAnim: TMenuItem;
            miSeparator: TMenuItem;
            miPrevLegsAnim: TMenuItem;
            miNextLegsAnim: TMenuItem;

            procedure FormCreate(pSender: TObject);
            procedure FormKeyPress(pSender: TObject; var Key: Char);
            procedure FormPaint(pSender: TObject);
            procedure FormResize(pSender: TObject);
            procedure miPrevTorsoAnimClick(pSender: TObject);
            procedure miNextTorsoAnimClick(pSender: TObject);
            procedure miPrevLegsAnimClick(pSender: TObject);
            procedure miNextLegsAnimClick(pSender: TObject);

        private type
            {**
             Frame structure, contains the local model cache
            }
            IFrame = class
                private
                    m_pMesh:     TQRMesh;
                    m_pAABBTree: TQRAABBTree;

                public
                    {**
                     Constructor
                     @param(useCollisions - if @true, collisions detection is enabled)
                    }
                    constructor Create(useCollisions: Boolean); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            IFrames = TObjectDictionary<NativeUInt, IFrame>;

        private

        public
    end;

var
  MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
// TMainForm.IFrame
//--------------------------------------------------------------------------------------------------
constructor TMainForm.IFrame.Create(useCollisions: Boolean);
begin
    inherited Create;

    if (useCollisions) then
        m_pAABBTree := TQRAABBTree.Create
    else
        m_pAABBTree := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.IFrame.Destroy;
begin
    m_pAABBTree.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormCreate(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormKeyPress(pSender: TObject; var Key: Char);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormPaint(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormResize(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miPrevTorsoAnimClick(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miNextTorsoAnimClick(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miPrevLegsAnimClick(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.miNextLegsAnimClick(pSender: TObject);
begin
//
end;
//--------------------------------------------------------------------------------------------------

end.
