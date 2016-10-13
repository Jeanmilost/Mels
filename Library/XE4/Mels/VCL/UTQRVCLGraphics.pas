{**************************************************************************************************
 * ==> UTQRVCLGraphics ---------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This unit provides common tools to support some common graphic tasks that the    *
 *               VCL don't care.                                                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLGraphics;

interface

uses System.Classes,
     System.SysUtils,
     UTQRGraphics,
     Vcl.Graphics,
     Winapi.Windows;

type
    {**
    * Universal color class
    *}
    TQRVCLColor = class(TQRColor)
        public
            {**
            * Constructor
            *}
            constructor Create(); overload; override;

            {**
            * Constructor
            *@param r - color red component intensity, from 0 (darkest) to 255 (lightest)
            *@param g - color green component intensity, from 0 (darkest) to 255 (lightest)
            *@param b - color blue component intensity, from 0 (darkest) to 255 (lightest)
            *}
            constructor Create(r, g, b: Byte); overload; override;

            {**
            * Constructor
            *@param r - color red component intensity, from 0 (darkest) to 255 (lightest)
            *@param g - color green component intensity, from 0 (darkest) to 255 (lightest)
            *@param b - color blue component intensity, from 0 (darkest) to 255 (lightest)
            *@param a - color alpha component intensity, from 0 (smallest) to 255 (highest)
            *}
            constructor Create(r, g, b, a: Byte); overload; override;

            {**
            * Constructor
            *@param pOther - color to copy from
            *}
            constructor Create(const pOther: TQRColor); overload; override;

            {**
            * Constructor
            *@param pOther - color to copy from
            *}
            constructor Create(const pOther: TQRVCLColor); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy(); override;

            {**
            * Gets VCL color
            *@return VCL color
            }
            function GetVCLColor(): TColor; virtual;

            {**
            * Sets VCL color
            *@param color - VCL color
            *@param alpha - color alpha component
            }
            procedure SetVCLColor(color: TColor; alpha: Byte); virtual;

            {**
            * Gets Windows color
            *@return VCL color
            }
            function GetWinColor(): COLORREF; virtual;

            {**
            * Sets Windows color
            *@param color - VCL color
            *@param alpha - color alpha component
            }
            procedure SetWinColor(color: COLORREF; alpha: Byte); virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLColor
//--------------------------------------------------------------------------------------------------
constructor TQRVCLColor.Create();
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
destructor TQRVCLColor.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLColor.GetVCLColor(): TColor;
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
function TQRVCLColor.GetWinColor(): COLORREF;
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
