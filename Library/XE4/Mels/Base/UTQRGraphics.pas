{**************************************************************************************************
 * ==> UTQRGraphics ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module includes some tools that facilitates common graphics manipulations.  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRGraphics;

interface

type
    {**
    * Universal color class
    *}
    TQRColor = class
        protected
            m_R: Byte;
            m_G: Byte;
            m_B: Byte;
            m_A: Byte;

        public
            {**
            * Constructor
            *}
            constructor Create(); overload; virtual;

            {**
            * Constructor
            *@param r - color red component intensity, from 0 (darkest) to 255 (lightest)
            *@param g - color green component intensity, from 0 (darkest) to 255 (lightest)
            *@param b - color blue component intensity, from 0 (darkest) to 255 (lightest)
            *}
            constructor Create(r, g, b: Byte); overload; virtual;

            {**
            * Constructor
            *@param r - color red component intensity, from 0 (darkest) to 255 (lightest)
            *@param g - color green component intensity, from 0 (darkest) to 255 (lightest)
            *@param b - color blue component intensity, from 0 (darkest) to 255 (lightest)
            *@param a - color alpha component intensity, from 0 (smallest) to 255 (highest)
            *}
            constructor Create(r, g, b, a: Byte); overload; virtual;

            {**
            * Constructor
            *@param pOther - other color to copy from
            *}
            constructor Create(const pOther: TQRColor); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy(); override;

            { Basic functions }
            function  IsEqual(const pOther: TQRColor): Boolean; virtual;
            function  Differs(const pOther: TQRColor): Boolean; virtual;
            procedure Assign(const pOther: TQRColor);          virtual;

            {**
            * Sets color
            *@param r - color red component
            *@param g - color green component
            *@param b - color blue component
            *@param a - color alpha component
            *}
            procedure SetColor(r, g, b: Byte);    overload; virtual;
            procedure SetColor(r, g, b, a: Byte); overload; virtual;

            {**
            * Gets color as 32 bit value
            *@return color as 32 bit value
            *}
            function GetRGB():  Cardinal; virtual;
            function GetBGR():  Cardinal; virtual;
            function GetARGB(): Cardinal; virtual;
            function GetRGBA(): Cardinal; virtual;
            function GetABGR(): Cardinal; virtual;
            function GetBGRA(): Cardinal; virtual;

            {**
            * Sets color from 32 bit value
            *@return color as 32 bit value
            *}
            procedure SetRGB(value: Cardinal);  virtual;
            procedure SetBGR(value: Cardinal);  virtual;
            procedure SetARGB(value: Cardinal); virtual;
            procedure SetRGBA(value: Cardinal); virtual;
            procedure SetABGR(value: Cardinal); virtual;
            procedure SetBGRA(value: Cardinal); virtual;

            {**
            * Gets red component
            *@return red component
            *}
            function GetRed(): Byte; virtual;

            {**
            * Gets green component
            *@return green component
            *}
            function GetGreen(): Byte; virtual;

            {**
            * Gets blue component
            *@return blue component
            *}
            function GetBlue(): Byte; virtual;

            {**
            * Gets alpha component
            *@return alpha component
            *}
            function GetAlpha(): Byte; virtual;

            {**
            * Sets red component
            *@param value - component value
            *}
            procedure SetRed(value: Byte); virtual;

            {**
            * Sets green component
            *@param value - component value
            *}
            procedure SetGreen(value: Byte); virtual;

            {**
            * Sets blue component
            *@param value - component value
            *}
            procedure SetBlue(value: Byte); virtual;

            {**
            * Sets alpha component
            *@param value - component value
            *}
            procedure SetAlpha(value: Byte); virtual;

            {**
            * Gets red component as float (between 0.0f and 1.0f)
            *@return red component
            *}
            function GetRedF(): Single; virtual;

            {**
            * Gets green component as float (between 0.0f and 1.0f)
            *@return green component
            *}
            function GetGreenF(): Single; virtual;

            {**
            * Gets blue component as float (between 0.0f and 1.0f)
            *@return blue component
            *}
            function GetBlueF(): Single; virtual;

            {**
            * Gets alpha component as float (between 0.0f and 1.0f)
            *@return alpha component
            *}
            function GetAlphaF(): Single; virtual;

            {**
            * Blend color with another color
            *@param pOther - other color to blend with
            *@param offset - blend offset (from 0.0f to 1.0f)
            *@return blended color
            *@note The blended color should be deleted when useless
            *}
            function Blend(const pOther: TQRColor; const offset: Single): TQRColor; virtual;

            { Properties }
            property R: Byte read m_R write m_R;
            property G: Byte read m_G write m_G;
            property B: Byte read m_B write m_B;
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
function TQRColor.GetRGB(): Cardinal;
begin
    Result := ((m_R shl 16) + (m_G shl 8) + m_B);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBGR(): Cardinal;
begin
    Result := ((m_B shl 16) + (m_G shl 8) + m_R);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetARGB(): Cardinal;
begin
    Result := ((m_A shl 24) + (m_R shl 16) + (m_G shl 8) + m_B);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetRGBA(): Cardinal;
begin
    Result := ((m_R shl 24) + (m_G shl 16) + (m_B shl 8) + m_A);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetABGR(): Cardinal;
begin
    Result := ((m_A shl 24) + (m_B shl 16) + (m_G shl 8) + m_R);
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBGRA(): Cardinal;
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
function TQRColor.GetRed(): Byte;
begin
    Result := m_R;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetGreen(): Byte;
begin
    Result := m_G;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBlue(): Byte;
begin
    Result := m_B;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetAlpha(): Byte;
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
function TQRColor.GetRedF(): Single;
begin
    Result := m_R / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetGreenF(): Single;
begin
    Result := m_G / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetBlueF(): Single;
begin
    Result := m_B / 255.0;
end;
//--------------------------------------------------------------------------------------------------
function TQRColor.GetAlphaF(): Single;
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
