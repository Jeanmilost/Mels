// *************************************************************************************************
// * ==> UTQRVCLHelpers ---------------------------------------------------------------------------*
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
 @abstract(@name provides some features that the VCL don't care.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRVCLHelpers;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     Masks,
     UTQRCommon,
     UTQRHelpers,
     Graphics,
     Windows;

type
    {$REGION 'Documentation'}
    {**
     Array of RGBA pixels
    }
    {$ENDREGION}
    TRGBQuadArray = array of TRGBQuad;

    {$REGION 'Documentation'}
    {**
     Pointer to an array of RGBA pixels
    }
    {$ENDREGION}
    PRGBQuadArray = ^TRGBQuadArray;

    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate strings in a VCL context
    }
    {$ENDREGION}
    TQRVCLStringHelper = record
        {$REGION 'Documentation'}
        {**
         Gets text from resource
         @param(hInstance Module instance containing the text to get)
         @param(resourceName Resource name)
         @return(Text, empty string on error or if not found)
        }
        {$ENDREGION}
        class function FromResource(hInstance: NativeUInt;
                           const resourceName: string): string; static;
    end;

    PQRVCLStringHelper = ^TQRVCLStringHelper;

    // @exclude(needed, because these data types are ignored and not compiled in Vcl.Graphics if CRL
    //          is defined, although they are useful)
    TQRRGBTripleArray = array [Byte] of TRGBTriple;
    PQRRGBTripleArray = ^TQRRGBTripleArray;
    TQRRGBQuadArray   = array [Byte] of TRGBQuad;
    PQRRGBQuadArray   = ^TQRRGBQuadArray;

    {$REGION 'Documentation'}
    {**
     Targa (.tga) file header structure
    }
    {$ENDREGION}
    TQRTGAHeader = record
        {$REGION 'Documentation'}
        {**
         Size of identifier that follows this 18 byte long header
        }
        {$ENDREGION}
        m_IDSize: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Color map type, values are:
         @br 0 - No palette
         @br 1 - Has palette
        }
        {$ENDREGION}
        m_ColorMapType: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image type, values are:
         @br 0  - No image data
         @br 1  - Uncompressed, indexed
         @br 2  - Uncompressed, True Color (RGB)
         @br 3  - Uncompressed, Grey
         @br 9  - Run-Length encoded, indexed
         @br 10 - Run-Length encoded, True Color (RGB)
         @br 11 - Run-Length encoded, Grey
        }
        {$ENDREGION}
        m_ImageType: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         First color map entry in palette
        }
        {$ENDREGION}
        m_ColorMapFirstIndex: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Palette color count
        }
        {$ENDREGION}
        m_ColorMapLength: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Bits per palette entry count, values are: 15, 16, 24 or 32
        }
        {$ENDREGION}
        m_ColorMapEntrySize: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image origin on X axis, in pixels
        }
        {$ENDREGION}
        m_OrigX: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image origin on Y axis, in pixels
        }
        {$ENDREGION}
        m_OrigY: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image width in pixels
        }
        {$ENDREGION}
        m_Width: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image height in pixels
        }
        {$ENDREGION}
        m_Height: array [0..1] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image bits per pixels, values are: 8, 16, 24 or 32
        }
        {$ENDREGION}
        m_BPP: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image descriptor bits (vh flip bits)
        }
        {$ENDREGION}
        m_ImageInfo: TQRUInt8;
    end;

    PQRTGAHeader = ^TQRTGAHeader;

    {$REGION 'Documentation'}
    {**
     Personal computer exchange (.pcx) file header structure
    }
    {$ENDREGION}
    TQRPCXHeader = record
        {$REGION 'Documentation'}
        {**
         PCX ID number (always $0A)
        }
        {$ENDREGION}
        m_Identifier: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Version number
        }
        {$ENDREGION}
        m_Version: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Encoding format
        }
        {$ENDREGION}
        m_Encoding: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Bit per pixel
        }
        {$ENDREGION}
        m_BitsPerPixel: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Image left position, in pixels
        }
        {$ENDREGION}
        m_XStart: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Image top position, in pixels
        }
        {$ENDREGION}
        m_YStart: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Image right position, in pixels
        }
        {$ENDREGION}
        m_XEnd: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Image bottom position, in pixels
        }
        {$ENDREGION}
        m_YEnd: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Horizontal resolution, in pixels
        }
        {$ENDREGION}
        m_HorzRes: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Vertical resolution, in pixels
        }
        {$ENDREGION}
        m_VertRes: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         16 color EGA palette
        }
        {$ENDREGION}
        m_Palette: array [0..47] of TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Reserved (always 0)
        }
        {$ENDREGION}
        m_Reserved1: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Bit planes number
        }
        {$ENDREGION}
        m_NumBitPlanes: TQRUInt8;

        {$REGION 'Documentation'}
        {**
         Bytes per scanline
        }
        {$ENDREGION}
        m_BytesPerLine: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Palette type
        }
        {$ENDREGION}
        m_PaletteType: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Horizontal screen size, in pixels
        }
        {$ENDREGION}
        m_HorzScreenSize: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Vertical screen size, in pixels
        }
        {$ENDREGION}
        m_VertScreenSize: TQRUInt16;

        {$REGION 'Documentation'}
        {**
         Reserved (always 0)
        }
        {$ENDREGION}
        m_Reserved2: array [0..53] of TQRUInt8;
    end;

    PQRPCXHeader = ^TQRPCXHeader;

    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate pictures
    }
    {$ENDREGION}
    TQRVCLPictureHelper = record
        private
            {$REGION 'Documentation'}
            {**
             Copies a pixel from source to destination
             @param(pSrc Source pixel to copy)
             @param(pDest Destination pixel)
            }
            {$ENDREGION}
            class procedure CopyPixel(const pSrc, pDst: Pointer); static;

            {$REGION 'Documentation'}
            {**
             Copies a pixel from source to dest and swaps the RGB color values
             @param(pSrc Source pixel to swap and copy)
             @param(pDest Destination pixel)
            }
            {$ENDREGION}
            class procedure CopySwapPixel(const pSrc, pDst: Pointer); static;

        public
            {$REGION 'Documentation'}
            {**
             Extracts file name without extension
             @param(fileName File name to extract from)
             @return(File name without extension, empty string if not found)
            }
            {$ENDREGION}
            class function IsGraphicClassRegistered(const fileName: TFileName): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Converts picture content to bitmap
             @param(pPicture Source picture)
             @param(pBitmap Destination bitmap)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function ToBitmap(const pPicture: TPicture; pBitmap: Graphics.TBitmap): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Checks if a pacture contains a bitmap or image in another format
             @param(pPicture Picture to check)
             @return(@true if picture contains a bitmap, otherwise @false)
             @br @bold(NOTE)This function is required, because accessing directly to the picture
                            Bitmap property will force picture to create a bitmap and thus delete
                            the existing picture
            }
            {$ENDREGION}
            class function IsBitmap(const pPicture: TPicture): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Gets size of any image type contained in the picture
             @param(pPicture Picture to get size from)
             @param(width @bold([out]) Picture width)
             @param(height @bold([out]) Picture height)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function GetPictureSize(const pPicture: TPicture;
                                       out width, height: Integer): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Gets pixel format from bitmap
             @param(pBitmap Bitmap to get from)
             @return(Bit count, 0 if not found or unknown)
            }
            {$ENDREGION}
            class function GetBitmapPixelFormat(const pBitmap: Graphics.TBitmap): Integer; static;

            {$REGION 'Documentation'}
            {**
             Gets bytes representing pixels from bitmap
             @param(pBitmap Bitmap to extract bytes from)
             @param(pPixels @bold([in, out]) Byte array to contain bitmap pixels, should be initialized)
             @param(flipY If @true, image will be mirrored on the y axis)
             @param(bgr If @true, resulting pixels will be written in BGR format, RGB otherwise)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Buffer containing bytes will be initialized internally, and should be
                             deleted using e.g. SetLength(pPixels, 0) when useless
            }
            {$ENDREGION}
            class function BytesFromBitmap(const pBitmap: Graphics.TBitmap;
                                             var pPixels: TQRByteArray;
                                              flipY, bgr: Boolean): Boolean; overload; static;

            {$REGION 'Documentation'}
            {**
             Gets bytes representing pixels from bitmap
             @param(pBitmap Bitmap to extract bytes from)
             @return(Bytes array, @nil on error)
             @br @bold(NOTE) Returned bytes should be deleted when useless
             @br @bold(NOTE) @bold(WARNING) this function overload was never tested
            }
            {$ENDREGION}
            class function BytesFromBitmap(const pBitmap: Graphics.TBitmap): Pointer; overload; static;

            {$REGION 'Documentation'}
            {**
             Builds a bitmap from a byte array
             @param(pPixels Byte array containing image pixels)
             @param(width Image width)
             @param(height Image height)
             @param(bpp Bytes per pixels (should be 24 or 32, other values are unsupported for now))
             @param(flipY If @true, image will be mirrored on the y axis)
             @param(pBitmap Bitmap that will contain the image)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function BitmapFromBytes(const pPixels: Pointer;
                                      width, height, bpp: Cardinal;
                                                   flipY: Boolean;
                                                 pBitmap: Graphics.TBitmap): Boolean; static;

            {$REGION 'Documentation'}
            {**
             Loads targa (.tga) image from file
             @param(fileName Image file name to load)
             @param(swapRGB If @true, RGB color will be swapped to BGR)
             @param(pBitmap Bitmap that will receive the image)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function LoadTGA(const fileName: TFileName;
                                          swapRGB: Boolean;
                                          pBitmap: Graphics.TBitmap): Boolean; overload; static;

            {$REGION 'Documentation'}
            {**
             Loads targa (.tga) image from stream
             @param(pStream Stream containing image data to load)
             @param(readLength Length, in bytes, to read in buffer)
             @param(swapRGB If @true, RGB color will be swapped to BGR)
             @param(pBitmap Bitmap that will receive the image)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Read will start from current buffer position
            }
            {$ENDREGION}
            class function LoadTGA(const pStream: TStream;
                                      readLength: NativeUInt;
                                         swapRGB: Boolean;
                                         pBitmap: Graphics.TBitmap): Boolean; overload; static;

            {$REGION 'Documentation'}
            {**
             Loads personal computer exchange (.pcx) image from file
             @param(fileName Image file name to load)
             @param(pBitmap Bitmap that will receive the image)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            class function LoadPCX(const fileName: TFileName;
                                          pBitmap: Graphics.TBitmap): Boolean; overload; static;

            {$REGION 'Documentation'}
            {**
             Loads personal computer exchange (.pcx) image from stream
             @param(pStream Stream containing image data to load)
             @param(readLength Length, in bytes, to read in buffer)
             @param(pBitmap Bitmap that will receive the image)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Read will start from current buffer position
            }
            {$ENDREGION}
            class function LoadPCX(const pStream: TStream;
                                      readLength: NativeUInt;
                                         pBitmap: Graphics.TBitmap): Boolean; overload; static;
    end;

    PQRVCLPictureHelper = ^TQRVCLPictureHelper;

    {$REGION 'Documentation'}
    {**
     Helper to manipulate the Windows Graphics Device Interface (GDI)
    }
    {$ENDREGION}
    TQRGDIHelper = record
        {$REGION 'Documentation'}
        {**
         Applies antialiasing from a source bitmap to a destination bitmap
         @param(pSource Source bitmap)
         @param(pDest Destination bitmap)
         @param(factor Antialiasing factor, should match with the difference of size between source
                       and destination bitmaps (e.g. for a source of 400x400 pixels and a destination
                       of 100x100 pixels, the factor should be 4))
        }
        {$ENDREGION}
        class procedure ApplyAntialiasing(pSource, pDest: Graphics.TBitmap;
                                                  factor: NativeInt); overload; static;

        {$REGION 'Documentation'}
        {**
         Applies antialiasing from a source device context to a destination device context
         @param(hSrc Source device context)
         @param(hDst Destination device context)
         @param(x x position in pixels to copy from)
         @param(y y position in pixels to copy from)
         @param(width - width to copy from source to destination)
         @param(height Height to copy from source to destination)
         @param(factor Antialiasing factor, should match with the difference of size between source
                       and destination bitmaps (e.g. for a source of 400x400 pixels and a destination
                       of 100x100 pixels, the factor should be 4))
        }
        {$ENDREGION}
        class procedure ApplyAntialiasing(hSrc, hDst: THandle;
                         x, y, width, height, factor: NativeInt); overload; static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLStringHelper
//--------------------------------------------------------------------------------------------------
class function TQRVCLStringHelper.FromResource(hInstance: NativeUInt; const resourceName: string): string;
var
    pStream:       TResourceStream;
    pStringStream: TStringStream;
begin
    Result := '';

    if (hInstance = 0) then
        Exit;

    pStream       := nil;
    pStringStream := nil;

    try
        pStringStream := TStringStream.Create('');

        // found module and package contains the default vertex shader program?
        if (FindResource(hInstance, PChar(resourceName), RT_RCDATA) <> 0) then
        begin
            // load default vertex shader program from stream
            pStream := TResourceStream.Create(hInstance, PChar(resourceName), RT_RCDATA);

            // convert resource file to text
            pStringStream.CopyFrom(pStream, 0);
            Result := string(pStringStream.DataString);
        end;
    finally
        // clear memory
        pStringStream.Free;
        pStream.Free
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLPictureHelper
//--------------------------------------------------------------------------------------------------
{$IFDEF CPUX64}
    class procedure TQRVCLPictureHelper.CopyPixel(const pSrc, pDst: Pointer);
    begin
        CopyMemory(pDst, pSrc, SizeOf(Byte) * 4)
    end;
{$ELSE}
    class procedure TQRVCLPictureHelper.CopyPixel(const pSrc, pDst: Pointer);
    asm
        push ebx
        mov bl, [eax + 0]
        mov bh, [eax + 1]
        mov [edx + 0], bl
        mov [edx + 1], bh
        mov bl, [eax + 2]
        mov bh, [eax + 3]
        mov [edx + 2], bl
        mov [edx + 3], bh
        pop ebx
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
{$IFDEF CPUX64}
    class procedure TQRVCLPictureHelper.CopySwapPixel(const pSrc, pDst: Pointer);
    var
        r, g, b, a: Byte;
    begin
        b := (Cardinal(pSrc^) shr 24) and $FF;
        g := (Cardinal(pSrc^) shr 16) and $FF;
        r := (Cardinal(pSrc^) shr 8)  and $FF;
        a :=  Cardinal(pSrc^)         and $FF;

        PCardinal(pDst)^ := ((r shl 24) + (g shl 16) + (b shl 8) + a);
    end;
{$ELSE}
    class procedure TQRVCLPictureHelper.CopySwapPixel(const pSrc, pDst: Pointer);
    asm
        push ebx
        mov bl, [eax + 0]
        mov bh, [eax + 1]
        mov [edx + 2], bl
        mov [edx + 1], bh
        mov bl, [eax + 2]
        mov bh, [eax + 3]
        mov [edx + 0], bl
        mov [edx + 3], bh
        pop ebx
    end;
{$ENDIF}
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.IsGraphicClassRegistered(const fileName: TFileName): Boolean;
var
    list: TStringList;
    i:    Integer;
begin
    Result := False;
    list   := TStringList.Create;

    try
        list.Delimiter       := ';';
        list.StrictDelimiter := True;
        list.DelimitedText   := GraphicFileMask(TGraphic);

        if (list.Count = 0) then
            Exit;

        for i := 0 to list.Count - 1 do
        begin
            if MatchesMask(fileName, list[i]) then
            begin
                Result := True;
                Exit;
            end;
        end;
    finally
        list.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.ToBitmap(const pPicture: TPicture; pBitmap: Graphics.TBitmap): Boolean;
begin
    // no source picture?
    if (not Assigned(pPicture)) then
    begin
        Result := False;
        Exit;
    end;

    // no destination bitmap?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    pBitmap.Width  := pPicture.Width;
    pBitmap.Height := pPicture.Height;
    pBitmap.Canvas.Draw(0, 0, pPicture.Graphic);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.IsBitmap(const pPicture: TPicture): Boolean;
var
    pGraphic: TGraphic;
begin
    // no picture to check?
    if (not Assigned(pPicture)) then
    begin
        // undefined
        Result := False;
        Exit;
    end;

    // get picture graphic. DO NOT try to get picture bitmap property, otherwise existing image will
    // be erased
    pGraphic := pPicture.Graphic;

    // picture contains a bitmap if the graphic is assigned and is a type of bitmap
    Result := (Assigned(pGraphic) and (pGraphic is Graphics.TBitmap));
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.GetPictureSize(const pPicture: TPicture; out width, height: Integer): Boolean;
begin
    width  := 0;
    height := 0;

    // no picture to get from?
    if (not Assigned(pPicture)) then
    begin
        // undefined
        Result := False;
        Exit;
    end;

    // is picture a bitmap?
    if (IsBitmap(pPicture)) then
    begin
        // in case of bitmap, picture width and height can be used directly
        width  := pPicture.Width;
        height := pPicture.Height;
        Result := True;
        Exit;
    end;

    // otherwise use internal graphic, if possible
    if (Assigned(pPicture.Graphic)) then
    begin
        width  := pPicture.Graphic.Width;
        height := pPicture.Graphic.Height;
        Result := True;
        Exit;
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.GetBitmapPixelFormat(const pBitmap: Graphics.TBitmap): Integer;
begin
    // no bitmap defined?
    if (not Assigned(pBitmap)) then
    begin
        Result := 0;
        Exit;
    end;

    // search for bitmap pixel format
    case pBitmap.PixelFormat of
        pfDevice:
            // unknown
            Result := 0;

        pf1bit:
            Result := 1;

        pf4bit:
            Result := 4;

        pf8bit:
            Result := 8;

        pf15bit:
            Result := 15;

        pf16bit:
            Result := 16;

        pf24bit:
            Result := 24;

        pf32bit:
            Result := 32;
    else
        // other formats are unknown or not used
        Result := 0;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.BytesFromBitmap(const pBitmap: Graphics.TBitmap;
                                                     var pPixels: TQRByteArray;
                                                      flipY, bgr: Boolean): Boolean;
var
    offset, offsetX:                          NativeUInt;
    pixelSize, lineSize, x, y, width, height: Cardinal;
    pLineRGB:                                 PQRRGBTripleArray;
    pLineRGBA:                                PRGBQuadArray;
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
    lineSize := width * pixelSize;

    // create pixels buffer
    SetLength(pPixels, height * lineSize);

    // iterate through bitmap lines
    for y := 0 to height - 1 do
    begin
        // calculate next offset
        if (flipY) then
            offset := ((height - 1) - y) * lineSize
        else
            offset := y * lineSize;

        // is 24 or 32 bit bitmap?
        if (pBitmap.PixelFormat = pf24bit) then
        begin
            // get pixels line from bitmap
            pLineRGB := pBitmap.ScanLine[y];

            // do swap pixels?
            if (bgr) then
                // memory copy 24 bit pixels line, as pixels are already in RGB format
                CopyMemory(@pPixels[offset], pLineRGB, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (x * pixelSize);

                    // copy and swap pixel
                    pPixels[offsetX]     := pLineRGB[x].rgbtRed;
                    pPixels[offsetX + 1] := pLineRGB[x].rgbtGreen;
                    pPixels[offsetX + 2] := pLineRGB[x].rgbtBlue;
                end;
        end
        else
        begin
            // get pixels line from bitmap
            pLineRGBA := pBitmap.ScanLine[y];

            // do swap pixels?
            if (bgr) then
                // memory copy 32 bit pixels line, as pixels are already in RGB format
                CopyMemory(@pPixels[offset], pLineRGBA, lineSize)
            else
                // iterate through line pixels
                for x := 0 to width - 1 do
                begin
                    // calculate next pixel offset
                    offsetX := offset + (x * pixelSize);

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
class function TQRVCLPictureHelper.BytesFromBitmap(const pBitmap: Graphics.TBitmap): Pointer;
var
    pixelFormat: Integer;
    bm:          TBitmapInfo;
    pBytes:      Pointer;
    hDC:         NativeUInt;
begin
    Result := nil;

    // no source bitmap?
    if (not Assigned(pBitmap)) then
        Exit;

    // get bitmap pixel format
    pixelFormat := GetBitmapPixelFormat(pBitmap);

    // is pixel format compatible? (can be 24 or 32 bit)
    if ((pixelFormat <> 24) and (pixelFormat <> 32)) then
        Exit;

    // configure bitmap header
    bm.bmiHeader.biWidth         := pBitmap.Width;
    bm.bmiHeader.biHeight        := pBitmap.Height;
    bm.bmiHeader.biPlanes        := 1;
    bm.bmiHeader.biBitCount      := pixelFormat;
    bm.bmiHeader.biCompression   := BI_RGB;
    bm.bmiHeader.biSizeImage     := 0;
    bm.bmiHeader.biXPelsPerMeter := 0;
    bm.bmiHeader.biYPelsPerMeter := 0;
    bm.bmiHeader.biClrUsed       := 0;
    bm.bmiHeader.biClrImportant  := 0;
    bm.bmiHeader.biSize          := SizeOf(TBitmapInfoHeader);

    // 24 or 32 bit bitmap?
    if (pixelFormat = 24) then
        GetMem(pBytes, pBitmap.Width * pBitmap.Height * 3 * 3)
    else
        GetMem(pBytes, pBitmap.Width * pBitmap.Height * 4 * 4);

    hDC := 0;

    try
        hDC := GetDC(0);

        if (hDC = 0) then
            Exit;

        if (GetDIBits(hDC, pBitmap.Handle, 0, pBitmap.Height, pBytes, bm, DIB_RGB_COLORS) <> 0) then
            Result := pBytes
        else
            FreeMem(pBytes);
    finally
        if (hDC <> 0) then
            ReleaseDC(0, hDC);
    end;
end ;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.BitmapFromBytes(const pPixels: Pointer;
                                              width, height, bpp: Cardinal;
                                                           flipY: Boolean;
                                                         pBitmap: Graphics.TBitmap): Boolean;
var
    y, lineSize, offset: Cardinal;
    pLineRGB:            PRGBTriple;
    pLineRGBA:           PRGBQuad;
begin
    // no source bytes?
    if (not Assigned(pPixels)) then
    begin
        Result := False;
        Exit;
    end;

    // no destination image?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // no image size?
    if ((width = 0) or (height = 0)) then
    begin
        Result := False;
        Exit;
    end;

    // set destination image size
    pBitmap.SetSize(width, height);

    // dispatch bytes per pixels
    case (bpp) of
        24:
        begin
            // set bitmap pixel format
            pBitmap.PixelFormat := pf24bit;

            // calculate line size
            lineSize := width * (bpp div 8);

            offset := 0;

            // iterate through bitmap lines
            for y := 0 to height - 1 do
            begin
                // get pixels line from bitmap
                if (flipY) then
                    pLineRGB := PRGBTriple(pBitmap.ScanLine[(height - 1) - y])
                else
                    pLineRGB := PRGBTriple(pBitmap.ScanLine[y]);

                // memory copy 24 bit pixels line
                CopyMemory(pLineRGB, Pointer(NativeUInt(pPixels) + offset), lineSize);

                // go to next source line
                Inc(offset, lineSize);
            end;

            Result := True;
        end;

        32:
        begin
            // set bitmap pixel format
            pBitmap.PixelFormat := pf32bit;

            // calculate line size
            lineSize := width * (bpp div 8);

            offset := 0;

            // iterate through bitmap lines
            for y := 0 to height - 1 do
            begin
                // get pixels line from bitmap
                if (flipY) then
                    pLineRGBA := PRGBQuad(pBitmap.ScanLine[(height - 1) - y])
                else
                    pLineRGBA := PRGBQuad(pBitmap.ScanLine[y]);

                // memory copy 24 bit pixels line
                CopyMemory(pLineRGBA, Pointer(NativeUInt(pPixels) + offset), lineSize);

                // go to next source line
                Inc(offset, lineSize);
            end;

            Result := True;
        end
    else
        Result := False;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.LoadTGA(const fileName: TFileName;
                                                  swapRGB: Boolean;
                                                  pBitmap: Graphics.TBitmap): Boolean;
var
    pStream: TFileStream;
begin
    Result := False;

    // file not exists?
    if (not FileExists(fileName)) then
        Exit;

    // open file
    pStream := TFileStream.Create(fileName, fmOpenRead);

    try
        pStream.Position := 0;

        // load TGA as stream
        Result := LoadTGA(pStream, pStream.Size, swapRGB, pBitmap);
    finally
        pStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.LoadTGA(const pStream: TStream;
                                              readLength: NativeUInt;
                                                 swapRGB: Boolean;
                                                 pBitmap: Graphics.TBitmap): Boolean;
var
    header:         TQRTGAHeader;
    pImage:         Pointer;
    pCompImage:     Pointer;
    width, height:  Integer;
    colorDepth:     Integer;
    imageSize:      Integer;
    bufferIndex:    Integer;
    currentByte:    Integer;
    currentPixel:   Integer;
    i:              Integer;
    pFront:        ^Byte;
    pBack:         ^Byte;
    temp:           Byte;
    flipY:          Boolean;
begin
    // no destination bitmap?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // length is too small to read image?
    if (readLength < SizeOf(TQRTGAHeader)) then
    begin
        Result := False;
        Exit;
    end;

    // clear image
    GetMem(pImage, 0);
    GetMem(pCompImage, 0);

    header := Default(TQRTGAHeader);

    // read image header
    pStream.ReadBuffer(header, SizeOf(TQRTGAHeader));

    // is TGA 24/32 bit RGB or compressed TGA RGB?
    if ((header.m_ImageType <> 2) and (header.m_ImageType <> 10)) then
    begin
        Result := False;
        Exit;
    end;

    // is color mapped file?
    if (header.m_ColorMapType <> 0) then
    begin
        Result := False;
        Exit;
    end;

    // get the width, height and color depth
    width      := header.m_Width[0]  + header.m_Width[1]  * 256;
    height     := header.m_Height[0] + header.m_Height[1] * 256;
    colorDepth := header.m_BPP;
    imageSize  := width * height * (colorDepth div 8);
    flipY      := ((header.m_OrigY[0] <> 0) or (header.m_OrigY[1] <> 1));

    if (colorDepth < 24) then
    begin
        Result := False;
        Exit;
    end;

    try
        // allocate memory for image
        GetMem(pImage, imageSize);

        // is standard 24/32 bit TGA file?
        if (header.m_ImageType = 2) then
        begin
            // is enough length remaining to read pixels?
            if (imageSize > (pStream.Size - pStream.Position)) then
            begin
                Result := False;
                Exit;
            end;

            // read image pixels
            pStream.ReadBuffer(pImage^, imageSize);

            // TGAs are stored as BGR, and not RGB, so swap the R and B bytes. 32 bit TGA files
            // have alpha channel and gets loaded differently
            if (header.m_BPP = 24) then
            begin
                // do swap RGB colors to BGR?
                if (swapRGB) then
                    // iterate through each image pixel
                    for i := 0 to (width * height) - 1 do
                    begin
                        // swap pixel value (from RGB to BGR)
                        pFront  := Pointer(NativeUInt(pImage) + NativeUInt(i * 3));
                        pBack   := Pointer(NativeUInt(pImage) + NativeUInt(i * 3 + 2));
                        temp    := pFront^;
                        pFront^ := pBack^;
                        pBack^  := temp;
                    end;

                // create final image
                Result := BitmapFromBytes(pImage, width, height, 24, flipY, pBitmap);
            end
            else
            begin
                // do swap RGB colors to BGR?
                if (swapRGB) then
                    // iterate through each image pixel
                    for i := 0 to (width * height) - 1 do
                    begin
                        // swap pixel value (from RGB to BGR)
                        pFront  := Pointer(NativeUInt(pImage) + NativeUInt(i * 4));
                        pBack   := Pointer(NativeUInt(pImage) + NativeUInt(i * 4 + 2));
                        temp    := pFront^;
                        pFront^ := pBack^;
                        pBack^  := temp;
                    end;

                // create final image
                Result := BitmapFromBytes(pImage, width, height, 32, flipY, pBitmap);
            end;

            Exit;
        end;

        // is compressed 24/32 bit TGA file?
        if (header.m_ImageType = 10) then
        begin
            // is enough length remaining to read pixels?
            if (readLength - sizeOf(TQRTGAHeader) > (pStream.Size - pStream.Position)) then
            begin
                Result := False;
                Exit;
            end;

            colorDepth   := colorDepth div 8;
            currentByte  :=0;
            currentPixel :=0;
            bufferIndex  :=0;

            // read and uncompress image pixels
            GetMem(pCompImage, readLength - sizeOf(TQRTGAHeader));
            pStream.ReadBuffer(pCompImage^, readLength - SizeOf(TQRTGAHeader));

            // extract pixel information from compressed data
            repeat
                pFront := Pointer(NativeUInt(pCompImage) + NativeUInt(bufferIndex));
                Inc(bufferIndex);

                // do swap RGB colors to BGR?
                if (swapRGB) then
                begin
                    if (pFront^ < 128) then
                    begin
                        For i := 0 to pFront^ do
                        begin
                            CopySwapPixel(Pointer(NativeUInt(pCompImage) + NativeUInt(bufferIndex + i * colorDepth)),
                                          Pointer(NativeUInt(pImage)     + NativeUInt(currentByte)));
                            Inc(currentByte, colorDepth);
                            Inc(currentPixel);
                        end;

                        Inc(bufferIndex, (pFront^ + 1) * colorDepth);
                    end
                    else
                    begin
                        For i := 0 to pFront^ - 128 do
                        begin
                            CopySwapPixel(Pointer(NativeUInt(pCompImage) + NativeUInt(bufferIndex)),
                                          Pointer(NativeUInt(pImage)     + NativeUInt(currentByte)));
                            Inc(currentByte, colorDepth);
                            Inc(currentPixel);
                        end;

                        Inc(bufferIndex, colorDepth);
                    end;
                end
                else
                    if (pFront^ < 128) then
                    begin
                        For i := 0 to pFront^ do
                        begin
                            CopyPixel(Pointer(NativeUInt(pCompImage) + NativeUInt(bufferIndex + i * colorDepth)),
                                      Pointer(NativeUInt(pImage)     + NativeUInt(currentByte)));
                            Inc(currentByte, colorDepth);
                            Inc(currentPixel);
                        end;

                        Inc(bufferIndex, (pFront^ + 1) * colorDepth);
                    end
                    else
                    begin
                        For i := 0 to pFront^ - 128 do
                        begin
                            CopyPixel(Pointer(NativeUInt(pCompImage) + NativeUInt(bufferIndex)),
                                      Pointer(NativeUInt(pImage)     + NativeUInt(currentByte)));
                            Inc(currentByte, colorDepth);
                            Inc(currentPixel);
                        end;

                        Inc(bufferIndex, colorDepth);
                    end;
            until (currentPixel >= width * height);

            // create final image
            if (colorDepth = 3) then
                Result := BitmapFromBytes(pImage, width, height, 24, flipY, pBitmap)
            else
                Result := BitmapFromBytes(pImage, width, height, 32, flipY, pBitmap);

            Exit;
        end;
    finally
        // clear compressed image
        if (Assigned(pCompImage)) then
            FreeMem(pCompImage);

        // clear image
        if (Assigned(pImage)) then
            FreeMem(pImage);
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.LoadPCX(const fileName: TFileName;
                                                  pBitmap: Graphics.TBitmap): Boolean;
var
    pStream: TFileStream;
begin
    Result := False;

    // file not exists?
    if (not FileExists(fileName)) then
        Exit;

    // open file
    pStream := TFileStream.Create(fileName, fmOpenRead);

    try
        pStream.Position := 0;

        // load PCX as stream
        Result := LoadPCX(pStream, pStream.Size, pBitmap);
    finally
        pStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLPictureHelper.LoadPCX(const pStream: TStream;
                                              readLength: NativeUInt;
                                                 pBitmap: Graphics.TBitmap): Boolean;
var
    header:          TQRPCXHeader;
    pRGB:            PQRRGBTripleArray;
    dataSize,
    width,
    height,
    scanLineLength,
    linePaddingSize,
    pixelsPerLine,
    pixelCount,
    srcIndex,
    dstIndex,
    xPos,
    padding,
    i,
    j,
    x,
    y,
    xOffset:         NativeUInt;
    pixel,
    runCount,
    runValue:        TQRUInt8;
    entry:           TQRUInt16;
    color:           TQRUInt32;
    fileData,
    pixels:          array of TQRUInt8;
    palette:         array of TQRUInt32;
begin
    // no source stream?
    if (not Assigned(pStream)) then
    begin
        Result := False;
        Exit;
    end;

    try
        // not enough size in file to read header?
        if (SizeOf(TQRPCXHeader) >= readLength) then
        begin
            Result := False;
            Exit;
        end;

        header := Default(TQRPCXHeader);

        // read PCX header
        pStream.ReadBuffer(header, SizeOf(TQRPCXHeader));

        // not a PCX file?
        if (header.m_Identifier <> $0A) then
        begin
            Result := False;
            Exit;
        end;

        // read image data
        dataSize := readLength - SizeOf(TQRPCXHeader);
        SetLength(fileData, dataSize);
        pStream.Read(fileData[0], dataSize);

        // calculate image width and height, the scan line length and the line padding size
        width           := (header.m_XEnd - header.m_XStart) + 1;
        height          := (header.m_YEnd - header.m_YStart) + 1;
        scanLineLength  := (header.m_NumBitPlanes * header.m_BytesPerLine);
        linePaddingSize := (Int64(scanLineLength) * Int64(8 div LongWord(header.m_BitsPerPixel))) - width;

        // no width or height?
        if ((width = 0) or (height = 0)) then
        begin
            Result := False;
            Exit;
        end;

        // create buffer to contain pixel colors
        pixelsPerLine := (scanLineLength + linePaddingSize);
        pixelCount    := pixelsPerLine * height;
        SetLength(pixels, pixelCount);

        srcIndex := 0;
        dstIndex := 0;
        xPos     := 0;

        repeat
            // check buffer overrun
            if (srcIndex >= dataSize) then
            begin
                Result := False;
                Exit;
            end;

            // get next pixel
            pixel := fileData[srcIndex];
            Inc(srcIndex);

            // do repeat pixel many times? (i.e. uncompress PCX by reading run values)
            if ((pixel and $C0) = $C0) then
            begin
                // check buffer overrun
                if (srcIndex >= dataSize) then
                begin
                    Result := False;
                    Exit;
                end;

                // get run count and pixel value
                runCount := pixel and $3F;
                runValue := fileData[srcIndex];
                Inc(srcIndex);
            end
            else
            begin
                // 1 pixel code, run count is one, just get pixel value
                runCount := 1;
                runValue := pixel;
            end;

            // no run count?
            if (runCount = 0) then
            begin
                Result := False;
                Exit;
            end;

            padding := 0;

            // write the pixel run to pixels buffer
            for i := dstIndex to (dstIndex + (runCount - NativeUInt(1))) do
            begin
                // check buffer overrun
                if ((i + padding) >= pixelCount) then
                begin
                    Result := False;
                    Exit;
                end;

                // set next pixel
                pixels[i + padding] := runValue;
                Inc(xPos);

                // end of line reached?
                if (xPos >= pixelsPerLine) then
                begin
                    // add padding, if needed
                    if (linePaddingSize > 0) then
                        for j := 0 to linePaddingSize - 1 do
                        begin
                            pixels[i + padding] := $00;
                            Inc(padding);
                        end;

                    // reset x position
                    Dec(xPos, pixelsPerLine);
                end;
            end;

            Inc(dstIndex, runCount);
        until (dstIndex >= pixelCount);

        // create and initialize palette
        SetLength(palette, 256);
        FillChar(palette[0], 256, $00);

        // check if a VGA palette follows
        if (fileData[srcIndex] = $0C) then
        begin
            // skip palette signature
            Inc(srcIndex);

            // get the palette
            for entry := 0 to 255 do
            begin
                // check buffer overrun
                if (srcIndex + 3 > dataSize) then
                begin
                    Result := False;
                    Exit;
                end;

                // read RGB palette entry
                palette[entry] := (fileData[srcIndex]     shl 16) or
                                  (fileData[srcIndex + 1] shl 8)  or
                                   fileData[srcIndex + 2];

                Inc(srcIndex, 3);
            end;
        end
        else
        // otherwise check if there is enough space to read a palette (not standard case, probably a
        // corruption, try to save the palette)
        if ((dataSize - srcIndex) = 768) then
        begin
            // get the palette
            for entry := 0 to 255 do
            begin
                // check buffer overrun
                if (srcIndex + 3 > dataSize) then
                begin
                    Result := False;
                    Exit;
                end;

                // read RGB palette entry
                palette[entry] := (fileData[srcIndex]     shl 16) or
                                  (fileData[srcIndex + 1] shl 8)  or
                                   fileData[srcIndex + 2];

                Inc(srcIndex, 3);
            end;
        end
        else
            // get the EGA palette
            CopyMemory(@palette[0], @header.m_Palette, 48);

        pBitmap.PixelFormat := pf24bit;
        pBitmap.SetSize(width, height);

        // iterate through bitmap lines
        for y := 0 to height - 1 do
        begin
            // get bitmap line pixel array
            pRGB := PQRRGBTripleArray(pBitmap.ScanLine[y]);

            // calculate start x offset on next line
            xOffset := (y * (scanLineLength + linePaddingSize));

            // iterate through bitmap pixels
            for x := 0 to width - 1 do
            begin
                // get current pixel color
                color := palette[pixels[xOffset + x]];

                // set pixel in bitmap
                pRGB[x].rgbtRed   := ((color shr 16) and $FF);
                pRGB[x].rgbtGreen := ((color shr 8)  and $FF);
                pRGB[x].rgbtBlue  :=  (color         and $FF);
            end;
        end;
    finally
        // clear memory
        SetLength(palette, 0);
        SetLength(pixels, 0);
        SetLength(fileData, 0);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
// TQRGDIHelper
//--------------------------------------------------------------------------------------------------
class procedure TQRGDIHelper.ApplyAntialiasing(pSource, pDest: Graphics.TBitmap;
                                                       factor: NativeInt);
begin
    // no source bitmap?
    if (not Assigned(pSource)) then
        Exit;

    // no destination bitmap?
    if (not Assigned(pDest)) then
        Exit;

    // configure destination bitmap
    pDest.PixelFormat := pSource.PixelFormat;
    //FIXME pDest.AlphaFormat := pSource.AlphaFormat;
    pDest.SetSize(pSource.Width div factor, pSource.Height div factor);

    // apply antialiasing
    TQRGDIHelper.ApplyAntialiasing(pSource.Canvas.Handle,
                                   pDest.Canvas.Handle,
                                   0,
                                   0,
                                   pDest.Width,
                                   pDest.Height,
                                   factor);
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRGDIHelper.ApplyAntialiasing(hSrc, hDst: THandle;
                              x, y, width, height, factor: NativeInt);
var
    prevMode: Integer;
begin
    // no source device context?
    if (hSrc = 0) then
        Exit;

    // no destination device context?
    if (hDst = 0) then
        Exit;

    // set stretch mode to half tones (thus resizing will be smooth)
    prevMode := SetStretchBltMode(hDst, HALFTONE);

    try
        // apply antialiasing on the destination image
        StretchBlt(hDst,
                   x,
                   y,
                   width,
                   height,
                   hSrc,
                   0,
                   0,
                   width  * factor,
                   height * factor,
                   SRCCOPY);
    finally
        // restore previous stretch blit mode
        SetStretchBltMode(hDst, prevMode);
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
