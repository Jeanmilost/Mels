// *************************************************************************************************
// * ==> UTQRVCLModelRenderer ---------------------------------------------------------------------*
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
 @abstract(@name provides a basic interface to implement a model renderer using the VCL and the
            Windows API.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRVCLModelRenderer;

{$MODE Delphi}

interface

uses Graphics,
     Windows,
     UTQRVCLHelpers,
     UTQRModelRenderer;

type
    {$REGION 'Documentation'}
    {**
     Basic interface to implement a model renderer
    }
    {$ENDREGION}
    TQRVCLModelRenderer = class(TQRModelRenderer)
        protected
            {$REGION 'Documentation'}
            {**
             Sets the pixel format that target renderer will use
             @param(hDC Device context linked to renderer)
             @param(doubleBuffered If @true, rendering will be double buffered)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Depending on implementation, the GDI may fail to draw above or below
                             the scene if double buffering is enabled. This also means that, in this
                             case, the control background will be opaque
            }
            {$ENDREGION}
            function SetTargetPixelFormat(hDC: THandle; doubleBuffered: Boolean): Boolean; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Converts a bitmap in pixels byte array
             @param(pBitmap Bitmap to convert)
             @param(pPixels @bold([out]) Pixels array when function returns)
             @param(flipY If @true, the image will be mirrored on the Y axis)
             @param(bgr If @true, the image will be converted from RGB to BGR (or RGBA to ABGR) format)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) The caller is responsible to delete the pPixels buffer content after
                             use, by calling the FreeMem() function
            }
            {$ENDREGION}
            class function BytesFromBitmap(const pBitmap: Graphics.TBitmap;
                                             out pPixels: PByte;
                                              flipY, bgr: Boolean): Boolean; static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLModelRenderer
//--------------------------------------------------------------------------------------------------
constructor TQRVCLModelRenderer.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLModelRenderer.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLModelRenderer.SetTargetPixelFormat(hDC: THandle; doubleBuffered: Boolean): Boolean;
var
    pfd:         TPixelFormatDescriptor;
    pixelFormat: Integer;
    flags:       DWORD;
begin
    // no device context?
    if (hDC = 0) then
        Exit(False);

    // configure default flags
    flags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;

    // do use double buffering?
    if (doubleBuffered) then
        flags := flags or PFD_DOUBLEBUFFER
    else
        flags := flags or PFD_SUPPORT_GDI;

    with pfd do
    begin
        nSize           := SizeOf(TPixelFormatDescriptor);
        nVersion        := 1;
        dwFlags         := flags;
        iPixelType      := PFD_TYPE_RGBA;
        cColorBits      := 24;
        cRedBits        := 0;
        cRedShift       := 0;
        cGreenBits      := 0;
        cGreenShift     := 0;
        cBlueBits       := 0;
        cBlueShift      := 0;
        cAlphaBits      := 0;
        cAlphaShift     := 0;
        cAccumBits      := 0;
        cAccumRedBits   := 0;
        cAccumGreenBits := 0;
        cAccumBlueBits  := 0;
        cAccumAlphaBits := 0;
        cDepthBits      := 32;
        cStencilBits    := 0;
        cAuxBuffers     := 0;
        iLayerType      := PFD_MAIN_PLANE;
        bReserved       := 0;
        dwLayerMask     := 0;
        dwVisibleMask   := 0;
        dwDamageMask    := 0;
    end;

    // get best available pixel format
    pixelFormat := ChoosePixelFormat(hDC, @pfd);

    // set pixel format to use
    Result := SetPixelFormat(hDC, pixelFormat, @pfd);
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLModelRenderer.BytesFromBitmap(const pBitmap: Graphics.TBitmap;
                                                     out pPixels: PByte;
                                                      flipY, bgr: Boolean): Boolean;
var
    width, height, x, y:                  Integer;
    pixelSize, lineSize, offset, offsetX: NativeUInt;
    pLineRGB:                             PQRRGBTripleArray;
    pLineRGBA:                            PQRRGBQuadArray;
begin
    // no bitmap?
    if (not Assigned(pBitmap)) then
        Exit(False);

    // is bitmap empty?
    if ((pBitmap.Width <= 0) or (pBitmap.Height <= 0)) then
        Exit(False);

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
                    pPixels[offsetX]     := (pLineRGB^)[x].rgbtRed;
                    pPixels[offsetX + 1] := (pLineRGB^)[x].rgbtGreen;
                    pPixels[offsetX + 2] := (pLineRGB^)[x].rgbtBlue;
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
                    pPixels[offsetX]     := (pLineRGBA^)[x].rgbRed;
                    pPixels[offsetX + 1] := (pLineRGBA^)[x].rgbGreen;
                    pPixels[offsetX + 2] := (pLineRGBA^)[x].rgbBlue;
                    pPixels[offsetX + 3] := (pLineRGBA^)[x].rgbReserved;
                end;
        end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------

end.
