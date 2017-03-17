// *************************************************************************************************
// * ==> UTQRVCLGraphics --------------------------------------------------------------------------*
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
 @abstract(@name provides some graphical features that the VCL don't care.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLGraphics;

interface

uses System.Classes,
     System.SysUtils,
     UTQRGraphics,
     Vcl.Graphics,
     Winapi.Windows;

type
    {$REGION 'Documentation'}
    {**
     Universal color class
    }
    {$ENDREGION}
    TQRVCLColor = class(TQRColor)
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; overload; override;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(r Color red component intensity, from 0 (darkest) to 255 (lightest))
             @param(g Color green component intensity, from 0 (darkest) to 255 (lightest))
             @param(b Color blue component intensity, from 0 (darkest) to 255 (lightest))
            }
            {$ENDREGION}
            constructor Create(r, g, b: Byte); overload; override;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(r Color red component intensity, from 0 (darkest) to 255 (lightest))
             @param(g Color green component intensity, from 0 (darkest) to 255 (lightest))
             @param(b Color blue component intensity, from 0 (darkest) to 255 (lightest))
             @param(a Color alpha component intensity, from 0 (smallest) to 255 (highest))
            }
            {$ENDREGION}
            constructor Create(r, g, b, a: Byte); overload; override;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOther Color to copy from)
            }
            {$ENDREGION}
            constructor Create(const pOther: TQRColor); overload; override;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOther Color to copy from)
            }
            {$ENDREGION}
            constructor Create(const pOther: TQRVCLColor); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets the VCL color
             @return(The VCL color)
            }
            {$ENDREGION}
            function GetVCLColor: TColor; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the VCL color
             @param(color VCL color)
             @param(alpha Color alpha component)
            }
            {$ENDREGION}
            procedure SetVCLColor(color: TColor; alpha: Byte); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the Windows color
             @return(The Windows color)
            }
            {$ENDREGION}
            function GetWinColor: COLORREF; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the Windows color
             @param(color Windows color)
             @param(alpha Color alpha component)
            }
            {$ENDREGION}
            procedure SetWinColor(color: COLORREF; alpha: Byte); virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLColor
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create(r, g, b: Byte);
begin
    inherited Create(r, g, b);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create(r, g, b, a: Byte);
begin
    inherited Create(r, g, b, a);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create(const pOther: TQRColor);
begin
    inherited Create(pOther);
end;
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create(const pOther: TQRVCLColor);
begin
    inherited Create(pOther);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLColor.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLColor.GetVCLColor: TColor;
begin
    // build and return color
    Result := TColor((B shl 16) + (G shl 8) + R);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLColor.SetVCLColor(color: TColor; alpha: Byte);
var
    rgbColor: Cardinal;
begin
    // strip VCL palette information and keep RGB value
    rgbColor := ColorToRGB(color);

    // convert from TColor
    R :=  (rgbColor         and $ff);
    G := ((rgbColor shr 8)  and $ff);
    B := ((rgbColor shr 16) and $ff);
    A :=   alpha;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLColor.GetWinColor: COLORREF;
begin
    Result := RGB(R, G, B);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLColor.SetWinColor(color: COLORREF; alpha: Byte);
begin
    // convert from Windows color
    R :=  (color         and $ff);
    G := ((color shr 8)  and $ff);
    B := ((color shr 16) and $ff);
    A :=   alpha;
end;
//--------------------------------------------------------------------------------------------------

end.
