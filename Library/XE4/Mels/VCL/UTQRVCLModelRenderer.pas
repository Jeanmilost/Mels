{**************************************************************************************************
 * ==> UTQRVCLModelRenderer ----------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a basic interface to implement a model renderer using VCL   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLModelRenderer;

interface

uses UTQRVCLHelpers,
     UTQRModelRenderer,
     Vcl.Graphics,
     Winapi.Windows;

type
    {**
    * Basic interface to implement a model renderer
    *}
    TQRVCLModelRenderer = class(TQRModelRenderer)
        protected
            {**
            * Sets pixel format that target renderer will use
            *@param hDC - device context linked to renderer
            *@param doubleBuffered - if true, OpenGL rendering will be double buffered
            *@return true on success, otherwise false
            *@note The GDI cannot be used to draw above or below OpenGL scene if double buffering is
            *      enabled. This also means that the control background will be opaque
            *}
            function SetTargetPixelFormat(hDC: THandle; doubleBuffered: Boolean): Boolean; virtual;

        public
            {**
            * Constructor
            *}
            constructor Create; override;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            {**
            * Converts bitmap in pixels byte array
            *@param pBitmap - bitmap to convert
            *@param[out] pPixels - pixels array when function returns
            *@param flipY - if true, image will be mirrored on the Y axis
            *@param bgr - if true, image will be converted from RGB to BGR (or RGBA to ABGR) format
            *@return true on success, otherwise false
            *@note The caller is responsible to delete the pPixels buffer content after use, by
            *      calling the FreeMem() function
            *}
            class function BytesFromBitmap(const pBitmap: Vcl.Graphics.TBitmap;
                                             out pPixels: PBYTE;
                                              flipY, bgr: Boolean): Boolean; static;
    end;

implementation
//------------------------------------------------------------------------------
// TQRVCLModelRenderer
//------------------------------------------------------------------------------
constructor TQRVCLModelRenderer.Create;
begin
    inherited Create;
end;
//------------------------------------------------------------------------------
destructor TQRVCLModelRenderer.Destroy;
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRVCLModelRenderer.SetTargetPixelFormat(hDC: THandle; doubleBuffered: Boolean): Boolean;
var
    pfd:         PIXELFORMATDESCRIPTOR;
    pixelFormat: Integer;
    flags:       DWORD;
begin
    // no device context?
    if (hDC = 0) then
    begin
        Result := False;
        Exit;
    end;

    // configure default flags
    flags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;

    // do use double buffering?
    if (doubleBuffered) then
        flags := flags or PFD_DOUBLEBUFFER
    else
        flags := flags or PFD_SUPPORT_GDI;

    pfd.nSize           := SizeOf(PIXELFORMATDESCRIPTOR);
    pfd.nVersion        := 1;
    pfd.dwFlags         := flags;
    pfd.iPixelType      := PFD_TYPE_RGBA;
    pfd.cColorBits      := 24;
    pfd.cRedBits        := 0;
    pfd.cRedShift       := 0;
    pfd.cGreenBits      := 0;
    pfd.cGreenShift     := 0;
    pfd.cBlueBits       := 0;
    pfd.cBlueShift      := 0;
    pfd.cAlphaBits      := 0;
    pfd.cAlphaShift     := 0;
    pfd.cAccumBits      := 0;
    pfd.cAccumRedBits   := 0;
    pfd.cAccumGreenBits := 0;
    pfd.cAccumBlueBits  := 0;
    pfd.cAccumAlphaBits := 0;
    pfd.cDepthBits      := 32;
    pfd.cStencilBits    := 0;
    pfd.cAuxBuffers     := 0;
    pfd.iLayerType      := PFD_MAIN_PLANE;
    pfd.bReserved       := 0;
    pfd.dwLayerMask     := 0;
    pfd.dwVisibleMask   := 0;
    pfd.dwDamageMask    := 0;

    // get best available pixel format
    pixelFormat := ChoosePixelFormat(hDC, @pfd);

    // set pixel format to use
    Result := SetPixelFormat(hDC, pixelFormat, @pfd);
end;
//------------------------------------------------------------------------------
class function TQRVCLModelRenderer.BytesFromBitmap(const pBitmap: Vcl.Graphics.TBitmap;
                                                     out pPixels: PBYTE;
                                                      flipY, bgr: Boolean): Boolean;
var
    width, height, x, y:                  Integer;
    pixelSize, lineSize, offset, offsetX: NativeUInt;
    pLineRGB:                             PQRRGBTripleArray;
    pLineRGBA:                            PQRRGBQuadArray;
begin
    // no bitmap?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // is bitmap empty?
    if ((pBitmap.Width <= 0) or (pBitmap.Height <= 0)) then
    begin
        Result := False;
        Exit;
    end;

    // get bitmap size
    width  := pBitmap.Width;
    height := pBitmap.Height;

    // get pixel size
    if (pBitmap.PixelFormat = pf32bit) then
        pixelSize := SizeOf(TRGBQuad)
    else
        pixelSize := SizeOf(TRGBTriple);

    // calculate line size
    lineSize := NativeUInt(width) * pixelSize;

    // create pixels buffer
    GetMem(pPixels, NativeUInt(height) * lineSize);

    // iterate through bitmap lines
    for y := 0 to height - 1 do
    begin
        // calculate next offset
        if (flipY) then
            offset := NativeUInt((height - 1) - y) * lineSize
        else
            offset := NativeUInt(y) * lineSize;

        // is 24 or 32 bit bitmap?
        if (pBitmap.PixelFormat = pf24bit) then
        begin
            // get pixels line from bitmap
            pLineRGB := PQRRGBTripleArray(pBitmap.ScanLine[y]);

            // do swap pixels?
            if (bgr) then
                // memory copy 24 bit pixels line, as pixels are already in RGB format
                CopyMemory(Pointer(NativeUInt(pPixels) + offset), pLineRGB, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (NativeUInt(x) * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     := pLineRGB[x].rgbtRed;
                    pPixels[offsetX + 1] := pLineRGB[x].rgbtGreen;
                    pPixels[offsetX + 2] := pLineRGB[x].rgbtBlue;
                end;
        end
        else
        begin
            // get pixels line from bitmap
            pLineRGBA := PQRRGBQuadArray(pBitmap.ScanLine[y]);

            // do swap pixels?
            if (bgr) then
                // memory copy 32 bit pixels line, as pixels are already in RGB format
                CopyMemory(Pointer(NativeUInt(pPixels) + offset), pLineRGBA, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (NativeUInt(x) * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     := pLineRGBA[x].rgbRed;
                    pPixels[offsetX + 1] := pLineRGBA[x].rgbGreen;
                    pPixels[offsetX + 2] := pLineRGBA[x].rgbBlue;
                    pPixels[offsetX + 3] := pLineRGBA[x].rgbReserved;
                end;
        end;
    end;

    Result := True;
end;
//------------------------------------------------------------------------------

end.
