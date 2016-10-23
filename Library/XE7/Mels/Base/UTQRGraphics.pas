// *************************************************************************************************
// * ==> UTQRGraphics -----------------------------------------------------------------------------*
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
 @abstract(@name provides features to facilitate the work with colors and graphics.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRGraphics;

interface

type
    {$REGION 'Documentation'}
    {**
     Universal color class
    }
    {$ENDREGION}
    TQRColor = class
        private
            m_R: Byte;
            m_G: Byte;
            m_B: Byte;
            m_A: Byte;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(r Color red component intensity, from 0 (darkest) to 255 (lightest))
             @param(g Color green component intensity, from 0 (darkest) to 255 (lightest))
             @param(b Color blue component intensity, from 0 (darkest) to 255 (lightest))
            }
            {$ENDREGION}
            constructor Create(r, g, b: Byte); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(r Color red component intensity, from 0 (darkest) to 255 (lightest))
             @param(g Color green component intensity, from 0 (darkest) to 255 (lightest))
             @param(b Color blue component intensity, from 0 (darkest) to 255 (lightest))
             @param(a Color alpha component intensity, from 0 (smallest) to 255 (highest))
            }
            {$ENDREGION}
            constructor Create(r, g, b, a: Byte); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Copy constructor
             @param(pOther Other color to copy from)
            }
            {$ENDREGION}
            constructor Create(const pOther: TQRColor); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Assigns (i.e. copies) the content from another color
             @param(other Other color to copy from)
            }
            {$ENDREGION}
            procedure Assign(const pOther: TQRColor); virtual;

            {$REGION 'Documentation'}
            {**
             Compares the content of 2 colors and determines if they are equal
             @param(other Other color to compare with)
             @return(@true if colors are equal, otherwise @false)
            }
            {$ENDREGION}
            function IsEqual(const pOther: TQRColor): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Compares the content of 2 colors and determines if they are different
             @param(other Other color to compare with)
             @return(@true if colors differ, otherwise @false)
            }
            {$ENDREGION}
            function Differs(const pOther: TQRColor): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Sets color
             @param(r Color red component)
             @param(g Color green component)
             @param(b Color blue component)
            }
            {$ENDREGION}
            procedure SetColor(r, g, b: Byte); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Sets color
             @param(r Color red component)
             @param(g Color green component)
             @param(b Color blue component)
             @param(a Color alpha component)
            }
            {$ENDREGION}
            procedure SetColor(r, g, b, a: Byte); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as RGB 32 bit value
             @return(Color as RGB 32 bit value)
            }
            {$ENDREGION}
            function GetRGB: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as BGR 32 bit value
             @return(Color as BGR 32 bit value)
            }
            {$ENDREGION}
            function GetBGR: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as ARGB 32 bit value
             @return(Color as ARGB 32 bit value)
            }
            {$ENDREGION}
            function GetARGB: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as RGBA 32 bit value
             @return(Color as RGBA 32 bit value)
            }
            {$ENDREGION}
            function GetRGBA: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as ABGR 32 bit value
             @return(Color as ABGR 32 bit value)
            }
            {$ENDREGION}
            function GetABGR: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Gets color as BGRA 32 bit value
             @return(Color as BGRA 32 bit value)
            }
            {$ENDREGION}
            function GetBGRA: Cardinal; virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from RGB 32 bit value
             @param(value RGB 32 bit color value)
            }
            {$ENDREGION}
            procedure SetRGB(value: Cardinal);  virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from BGR 32 bit value
             @param(value BGR 32 bit color value)
            }
            {$ENDREGION}
            procedure SetBGR(value: Cardinal);  virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from ARGB 32 bit value
             @param(value ARGB 32 bit color value)
            }
            {$ENDREGION}
            procedure SetARGB(value: Cardinal); virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from RGBA 32 bit value
             @param(value RGBA 32 bit color value)
            }
            {$ENDREGION}
            procedure SetRGBA(value: Cardinal); virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from ABGR 32 bit value
             @param(value ABGR 32 bit color value)
            }
            {$ENDREGION}
            procedure SetABGR(value: Cardinal); virtual;

            {$REGION 'Documentation'}
            {**
             Sets color from BGRA 32 bit value
             @param(value BGRA 32 bit color value)
            }
            {$ENDREGION}
            procedure SetBGRA(value: Cardinal); virtual;

            {$REGION 'Documentation'}
            {**
             Gets red component
             @return(Red component)
            }
            {$ENDREGION}
            function GetRed: Byte; virtual;

            {$REGION 'Documentation'}
            {**
             Gets green component
             @return(Green component)
            }
            {$ENDREGION}
            function GetGreen: Byte; virtual;

            {$REGION 'Documentation'}
            {**
             Gets blue component
             @return(Blue component)
            }
            {$ENDREGION}
            function GetBlue: Byte; virtual;

            {$REGION 'Documentation'}
            {**
             Gets alpha component
             @return(Alpha component)
            }
            {$ENDREGION}
            function GetAlpha: Byte; virtual;

            {$REGION 'Documentation'}
            {**
             Sets red component
             @param(Value Component value)
            }
            {$ENDREGION}
            procedure SetRed(value: Byte); virtual;

            {$REGION 'Documentation'}
            {**
             Sets green component
             @param(Value Component value)
            }
            {$ENDREGION}
            procedure SetGreen(value: Byte); virtual;

            {$REGION 'Documentation'}
            {**
             Sets blue component
             @param(Value Component value)
            }
            {$ENDREGION}
            procedure SetBlue(value: Byte); virtual;

            {$REGION 'Documentation'}
            {**
             Sets alpha component
             @param(Value Component value)
            }
            {$ENDREGION}
            procedure SetAlpha(value: Byte); virtual;

            {$REGION 'Documentation'}
            {**
             Gets red component as float (between 0.0 and 1.0)
             @return(Red component as float value)
            }
            {$ENDREGION}
            function GetRedF: Single; virtual;

            {$REGION 'Documentation'}
            {**
             Gets green component as float (between 0.0 and 1.0)
             @return(Green component as float value)
            }
            {$ENDREGION}
            function GetGreenF: Single; virtual;

            {$REGION 'Documentation'}
            {**
             Gets blue component as float (between 0.0 and 1.0)
             @return(Blue component as float value)
            }
            {$ENDREGION}
            function GetBlueF: Single; virtual;

            {$REGION 'Documentation'}
            {**
             Gets alpha component as float (between 0.0 and 1.0)
             @return(Alpha component as float value)
            }
            {$ENDREGION}
            function GetAlphaF: Single; virtual;

            {$REGION 'Documentation'}
            {**
             Blends a color with another color
             @param(pOther Other color to blend with)
             @param(offset Blend offset (from 0.0 to 1.0))
             @return(Blended color)
             @br @bold(NOTE) The blended color should be deleted when useless
            }
            {$ENDREGION}
            function Blend(const pOther: TQRColor; const offset: Single): TQRColor; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the color red component value
            }
            {$ENDREGION}
            property R: Byte read m_R write m_R;

            {$REGION 'Documentation'}
            {**
             Gets or sets the color green component value
            }
            {$ENDREGION}
            property G: Byte read m_G write m_G;

            {$REGION 'Documentation'}
            {**
             Gets or sets the color blue component value
            }
            {$ENDREGION}
            property B: Byte read m_B write m_B;

            {$REGION 'Documentation'}
            {**
             Gets or sets the color alpha component value
            }
            {$ENDREGION}
            property A: Byte read m_A write m_A;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRColor
//--------------------------------------------------------------------------------------------------
constructor TQRColor.Create;
begin
    inherited Create;

    m_R := 0;
    m_G := 0;
    m_B := 0;
    m_A := 255;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRColor.Create(r, g, b: Byte);
begin
    inherited Create;

    m_R := r;
    m_G := g;
    m_B := b;
    m_A := 255;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRColor.Create(r, g, b, a: Byte);
begin
    inherited Create;

    m_R := r;
    m_G := g;
    m_B := b;
    m_A := a;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRColor.Create(const pOther: TQRColor);
begin
    inherited Create;

    Assign(pOther);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRColor.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.Assign(const pOther: TQRColor);
begin
    if (not Assigned(pOther)) then
    begin
        m_R := 0;
        m_G := 0;
        m_B := 0;
        m_A := 255;
        Exit;
    end;

    m_R := pOther.R;
    m_G := pOther.G;
    m_B := pOther.B;
    m_A := pOther.A;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.IsEqual(const pOther: TQRColor): Boolean;
begin
    Result := ((m_R = pOther.m_R) and (m_G = pOther.m_G) and (m_B = pOther.m_B) and (m_A = pOther.m_A));
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.Differs(const pOther: TQRColor): Boolean;
begin
    Result := ((m_R <> pOther.m_R) or (m_G <> pOther.m_G) or (m_B <> pOther.m_B) or (m_A <> pOther.m_A));
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetColor(r, g, b: Byte);
begin
    m_R := r;
    m_G := g;
    m_B := b;
    m_A := 255;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetColor(r, g, b, a: Byte);
begin
    m_R := r;
    m_G := g;
    m_B := b;
    m_A := a;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetRGB: Cardinal;
begin
    Result := ((m_R shl 16) + (m_G shl 8) + m_B);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBGR: Cardinal;
begin
    Result := ((m_B shl 16) + (m_G shl 8) + m_R);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetARGB: Cardinal;
begin
    Result := ((m_A shl 24) + (m_R shl 16) + (m_G shl 8) + m_B);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetRGBA: Cardinal;
begin
    Result := ((m_R shl 24) + (m_G shl 16) + (m_B shl 8) + m_A);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetABGR: Cardinal;
begin
    Result := ((m_A shl 24) + (m_B shl 16) + (m_G shl 8) + m_R);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBGRA: Cardinal;
begin
    Result := ((m_B shl 24) + (m_G shl 16) + (m_R shl 8) + m_A);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetRGB(value: Cardinal);
begin
    m_R := ((value shr 16) and $FF);
    m_G := ((value shr 8)  and $FF);
    m_B :=   value         and $FF;
    m_A := 255;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetBGR(value: Cardinal);
begin
    m_B := ((value shr 16) and $FF);
    m_G := ((value shr 8)  and $FF);
    m_R :=   value         and $FF;
    m_A := 255;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetARGB(value: Cardinal);
begin
    m_A := ((value shr 24) and $FF);
    m_R := ((value shr 16) and $FF);
    m_G := ((value shr 8)  and $FF);
    m_B :=   value         and $FF;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetRGBA(value: Cardinal);
begin
    m_R := ((value shr 24) and $FF);
    m_G := ((value shr 16) and $FF);
    m_B := ((value shr 8)  and $FF);
    m_A :=   value         and $FF;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetABGR(value: Cardinal);
begin
    m_A := ((value shr 24) and $FF);
    m_B := ((value shr 16) and $FF);
    m_G := ((value shr 8)  and $FF);
    m_R :=   value         and $FF;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetBGRA(value: Cardinal);
begin
    m_B := ((value shr 24) and $FF);
    m_G := ((value shr 16) and $FF);
    m_R := ((value shr 8)  and $FF);
    m_A :=   value         and $FF;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetRed: Byte;
begin
    Result := m_R;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetGreen: Byte;
begin
    Result := m_G;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBlue: Byte;
begin
    Result := m_B;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetAlpha: Byte;
begin
    Result := m_A;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetRed(value: Byte);
begin
    m_R := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetGreen(value: Byte);
begin
    m_G := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetBlue(value: Byte);
begin
    m_B := value;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRColor.SetAlpha(value: Byte);
begin
    m_A := value;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetRedF: Single;
begin
    Result := m_R / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetGreenF: Single;
begin
    Result := m_G / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBlueF: Single;
begin
    Result := m_B / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetAlphaF: Single;
begin
    Result := m_A / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.Blend(const pOther: TQRColor; const offset: Single): TQRColor;
var
    colorR, colorG, colorB, colorA: Single;
begin
    // do return internal color?
    if (offset <= 0.0) then
    begin
        Result := TQRColor.Create(Self);
        Exit;
    end;

    // do return other color?
    if (offset >= 1.0) then
    begin
        Result := TQRColor.Create(pOther);
        Exit;
    end;

    // calculate blended color components
    colorR := (m_R + offset * (pOther.m_R - m_R));
    colorG := (m_G + offset * (pOther.m_G - m_G));
    colorB := (m_B + offset * (pOther.m_B - m_B));
    colorA := (m_A + offset * (pOther.m_A - m_A));

    // return blended color
    Result := TQRColor.Create(PByte(@colorR)^, PByte(@colorG)^, PByte(@colorB)^, PByte(@colorA)^);
end;
//--------------------------------------------------------------------------------------------------

end.
