// *************************************************************************************************
// * ==> UTQRHelpers ------------------------------------------------------------------------------*
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
 @abstract(@name provides helpers to support some common low level tasks that Delphi don't care.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRHelpers;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     UTQRCommon;

const
    {$REGION 'Documentation'}
    {**
     Directory delimiter used on Windows systems. Chr($5C) = '\'
    }
    {$ENDREGION}
    CQR_Win_Dir_Delimiter = Chr($5C);

    {$REGION 'Documentation'}
    {**
     Directory delimiter used on Mac OS X systems. Chr($2F) = '/'
    }
    {$ENDREGION}
    CQR_OSx_Dir_Delimiter = Chr($2F);

    {$REGION 'Documentation'}
    {**
     Directory delimiter used inside ZIP packages. Chr($2F) = '/'
    }
    {$ENDREGION}
    CQR_Zip_Dir_Delimiter = Chr($2F);

    {$REGION 'Documentation'}
    {**
     Platform independent direcvtory delimiter to use
    }
    {$ENDREGION}
    {$IF DEFINED(MSWINDOWS)}
        CQR_Dir_Delimiter = CQR_Win_Dir_Delimiter;
    {$ELSE IF DEFINED(MACOS) or DEFINED(IOS)}
        CQR_Dir_Delimiter = CQR_OSx_Dir_Delimiter;
    {$ELSE}
        {$MESSAGE FATAL 'Unknown platform type'}
    {$IFEND}
type
    {$REGION 'Documentation'}
    {**
     Byte array
    }
    {$ENDREGION}
    TQRByteArray = array of Byte;

    {$REGION 'Documentation'}
    {**
     Ansi char array
    }
    {$ENDREGION}
    TQRAnsiCharArray = array of AnsiChar;

    {$REGION 'Documentation'}
    {**
     Unicode char array
    }
    {$ENDREGION}
    TQRCharArray = Array of Char;

    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate strings
    }
    {$ENDREGION}
    TQRStringHelper = record
        {$REGION 'Documentation'}
        {**
         Converts array of ansi char to string
         @param(chars Array of ansi chars to convert)
         @return(String)
        }
        {$ENDREGION}
        class function AnsiCharArrayToStr(const chars: TQRAnsiCharArray): string; static;

        {$REGION 'Documentation'}
        {**
         Converts a string to an array of bytes
         @param(str String to convert)
         @return(Array of bytes)
        }
        {$ENDREGION}
        class function StrToByte(const str: string): TQRByteArray; static;

        {$REGION 'Documentation'}
        {**
         Converts an array of bytes to a string
         @param(bytes Array of bytes to convert)
         @return(String)
        }
        {$ENDREGION}
        class function ByteToStr(const bytes: TQRByteArray): string; static;

        {$REGION 'Documentation'}
        {**
         Converts an ansi string to an array of bytes
         @param(str String to convert)
         @return(Array of bytes)
        }
        {$ENDREGION}
        class function AnsiStrToByte(const str: AnsiString): TQRByteArray; static;

        {$REGION 'Documentation'}
        {**
         Converts an array of bytes to an ansi string
         @param(bytes Array of bytes to convert)
         @return(Ansi string)
        }
        {$ENDREGION}
        class function ByteToAnsiStr(const bytes: TQRByteArray): AnsiString; static;

        {$REGION 'Documentation'}
        {**
         Converts an Unicode string to an array of bytes
         @param(str String to convert)
         @return(Array of bytes)
        }
        {$ENDREGION}
        class function UniStrToByte(const str: UnicodeString): TQRByteArray; static;

        {$REGION 'Documentation'}
        {**
         Converts an array of bytes to an Unicode string
         @param(bytes Array of bytes to convert)
         @return(Unicode string)
        }
        {$ENDREGION}
        class function ByteToUniStr(const bytes: TQRByteArray): UnicodeString; static;

        {$REGION 'Documentation'}
        {**
         Checks if a digit contains only chars that can be converted to number
         @param(digit Digit to check)
         @param(strict If @true, only chars from '0' to '9' will be accepted, see note)
         @return(@true if digit contains only chars that can be converted to number, otherwise
                 @false)
         @br @bold(NOTE) If strict mode is set to false, math symbols as e.g. '.' or '-' will
                         also be accepted as valid numeric chars. This may be useful when
                         IsNumeric() is used e.g. to determine if a string can be converted to
                         number
        }
        {$ENDREGION}
        class function IsNumeric(digit: AnsiChar; isStrict: Boolean): Boolean; overload; static;

        {$REGION 'Documentation'}
        {**
         Checks if a digit contains only chars that can be converted to number
         @param(digit Digit to check)
         @param(strict If @true, only chars from '0' to '9' will be accepted, see note)
         @return(@true if digit contains only chars that can be converted to number, otherwise
                 @false)
         @br @bold(NOTE) If strict mode is set to false, math symbols as e.g. '.' or '-' will
                         also be accepted as valid numeric chars. This may be useful when
                         IsNumeric() is used e.g. to determine if a string can be converted to
                         number
        }
        {$ENDREGION}
        class function IsNumeric(digit: WideChar; isStrict: Boolean): Boolean; overload; static;

        {$REGION 'Documentation'}
        {**
         Searches the last position of a sub-string inside a string
         @param(subStr Sub-string to find)
         @param(str String containing the sub-string to find)
         @return(Last sub-string position inside the string, 0 if not found)
        }
        {$ENDREGION}
        class function LastPos(const subStr, str: UnicodeString): Integer; static;

        {$REGION 'Documentation'}
        {**
         Reverses a string content
         @param(str String to reverse)
         @return(Reversed string)
        }
        {$ENDREGION}
        class function Reverse(const str: UnicodeString): UnicodeString; static;
    end;

    PQRStringHelper = ^TQRStringHelper;

    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate files
    }
    {$ENDREGION}
    TQRFileHelper = record
        {$REGION 'Documentation'}
        {**
         Extracts file name
         @param(fileName File name to extract from)
         @param(delimiter Dir delimiter to use)
         @return(File name without extension, empty string if not found)
        }
        {$ENDREGION}
        class function ExtractFileName(const fileName: TFileName;
                                      const delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;

        {$REGION 'Documentation'}
        {**
         Extracts file name without extension
         @param(fileName File name to extract from)
         @param(delimiter Dir delimiter to use)
         @return(File name without extension, empty string if not found)
        }
        {$ENDREGION}
        class function ExtractFileNameNoExt(const fileName: TFileName;
                                           const delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;

        {$REGION 'Documentation'}
        {**
         Saves byte array to file
         @param(pBytes Bytes to save)
        }
        {$ENDREGION}
        class procedure SaveBytesToFile(const fileName: TFileName;
                                            var pBytes: TQRByteArray); static;

        {$REGION 'Documentation'}
        {**
         Appends delimiter at dir end
         @param(dirName Directory name to append to)
         @return(Appended dir name)
         @br @bold(NOTE) If delimiter already exists at end, it will not be appended again
        }
        {$ENDREGION}
        class function AppendDelimiter(const dirName: UnicodeString;
                                           delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;
    end;

    PQRFileHelper = ^TQRFileHelper;

    {$REGION 'Documentation'}
    {**
     Some helper functions for mathematics
    }
    {$ENDREGION}
    TQRMathsHelper = record
        {$REGION 'Documentation'}
        {**
         Checks if a value is a power of 2
         @param(x Value to check)
         @return(@true if value is a power of two, otherwise @false)
        }
        {$ENDREGION}
        class function IsPowerOfTwo(value: NativeUInt): Boolean; static;

        {$REGION 'Documentation'}
        {**
         Rounds up to the nearest power of 2
         @param(value Value to round up)
         @return(Rounded up power of 2)
        }
        {$ENDREGION}
        class function RoundUpToNearestPowerOf2(value: TQRUInt32): TQRUInt32; static;

        {$REGION 'Documentation'}
        {**
         Gets the closest power of 2 from a value
         @param(value Value)
         @return(Closest power of 2)
        }
        {$ENDREGION}
        class function GetClosestPowerOf2(value: NativeUInt): NativeUInt; static;

        {$REGION 'Documentation'}
        {**
         Converts degrees to radians
         @param(angle Angle in degrees)
         @return(Angle in radians)
        }
        {$ENDREGION}
        class function DegToRad(angle: Double): Double; static;

        {$REGION 'Documentation'}
        {**
         Converts radians to degrees
         @param(angle Angle in radians)
         @return(Angle in degrees)
        }
        {$ENDREGION}
        class function RadToDeg(angle: Double): Double; static;
    end;

    {$REGION 'Documentation'}
    {**
     Some helper functions to manipulate memory
    }
    {$ENDREGION}
    TQRMemoryHelper = record
        {$REGION 'Documentation'}
        {**
         Checks if system on which program is executed is big endian
         @return(@true if system on which program is executed is big endian, @false if little
                 endian)
        }
        {$ENDREGION}
        class function IsSystemBE: Boolean; static;

        {$REGION 'Documentation'}
        {**
         Swaps content of 2 variables
         @param(a [in, out] First variable to swap)
         @param(b [in, out] Second variable to swap)
        }
        {$ENDREGION}
        class procedure Swap<T>(var left, right: T); static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRStringHelper
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.AnsiCharArrayToStr(const chars: TQRAnsiCharArray): string;
begin
    // is char array empty?
    if (Length(chars) > 0) then
    begin
        // convert to string
        SetString(Result, PAnsiChar(@chars[0]), Length(chars));
        Exit;
    end;

    Result := '';
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.StrToByte(const str: string): TQRByteArray;
begin
    SetLength(Result, Length(str));

    if (Length(Result) > 0) then
        Move(str[1], Result[0], Length(Result));
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.ByteToStr(const bytes: TQRByteArray): string;
begin
    SetLength(Result, Length(bytes));

    if Length(Result) > 0 then
        Move(bytes[0], Result[1], Length(bytes));
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.AnsiStrToByte(const str: AnsiString): TQRByteArray;
begin
    SetLength(Result, Length(str) * SizeOf(AnsiChar));

    if (Length(Result) > 0) then
        Move(str[1], Result[0], Length(Result));
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.ByteToAnsiStr(const bytes: TQRByteArray): AnsiString;
begin
    SetLength(Result, Length(bytes) div SizeOf(AnsiChar));

    if Length(Result) > 0 then
        Move(bytes[0], Result[1], Length(bytes));
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.UniStrToByte(const str: UnicodeString): TQRByteArray;
begin
    SetLength(Result, Length(str) * SizeOf(WideChar));

    if (Length(Result) > 0) then
        Move(str[1], Result[0], Length(Result));
end;
//---------------------------------------------------------------------------
class function TQRStringHelper.ByteToUniStr(const bytes: TQRByteArray): UnicodeString;
begin
    SetLength(Result, Length(bytes) div SizeOf(WideChar));

    if Length(Result) > 0 then
        Move(bytes[0], Result[1], Length(bytes));
end;
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.IsNumeric(digit: AnsiChar; isStrict: Boolean): Boolean;
begin
    Result := (((digit >= '0') and (digit <= '9')) or ((not isStrict) and ((digit = '-') or (digit = '.'))));
end;
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.IsNumeric(digit: WideChar; isStrict: Boolean): Boolean;
begin
    Result := (((digit >= '0') and (digit <= '9')) or ((not isStrict) and ((digit = '-') or (digit = '.'))));
end;
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.LastPos(const subStr, str: UnicodeString): Integer;
begin
    Result := Pos(Reverse(subStr), Reverse(str));

    if (Result <> 0) then
        Result := ((Length(str) - Length(subStr)) + 1) - Result + 1;
end;
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.Reverse(const str: UnicodeString): UnicodeString;
var
    pCh1, pCh2: PWideChar;
begin
    SetLength(Result, Length(str));

    pCh1 := @str[1];
    pCh2 := @Result[Length(str)];

    while (pCh1^ <> #0) do
    begin
        pCh2^ := pCh1^;
        Dec(pCh2);
        Inc(pCh1);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRFileHelper
//--------------------------------------------------------------------------------------------------
class function TQRFileHelper.ExtractFileName(const fileName: TFileName;
                                            const delimiter: Char): UnicodeString;
var
    pos: Integer;
begin
    // get last delimiter position in the string
    pos := TQRStringHelper.LastPos(UnicodeString(delimiter), fileName);

    // found it?
    if (pos = 0) then
    begin
        Result := fileName;
        Exit;
    end;

    // extract file name
    Result := Copy(fileName, pos + 1);
end;
//--------------------------------------------------------------------------------------------------
class function TQRFileHelper.ExtractFileNameNoExt(const fileName: TFileName;
                                                 const delimiter: Char): UnicodeString;
begin
    Result := ChangeFileExt(ExtractFileName(fileName, delimiter), '');
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRFileHelper.SaveBytesToFile(const fileName: TFileName; var pBytes: TQRByteArray);
var
    pFileStream: TFileStream;
    pBytesPtr:   PByte;
begin
    pFileStream := nil;
    pBytesPtr   := @pBytes[0];

    try
        pFileStream := TFileStream.Create(fileName, fmOpenWrite);
        pFileStream.Write(pBytesPtr, Length(pBytes));
    finally
        // delete file stream
        pFileStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRFileHelper.AppendDelimiter(const dirName: UnicodeString;
                                                 delimiter: Char): UnicodeString;
begin
    // is name empty?
    if (Length(dirName) = 0) then
    begin
        SetLength(Result, 1);
        Result[1] := delimiter;
        Exit;
    end;

    // dir name already contains delimiter at end?
    if (dirName[Length(dirName)] = delimiter) then
    begin
        Result := dirName;
        Exit;
    end;

    // append delimiter
    Result := dirName + delimiter;
end;
//--------------------------------------------------------------------------------------------------
// TQRMathsHelper
//--------------------------------------------------------------------------------------------------
class function TQRMathsHelper.IsPowerOfTwo(value: NativeUInt): Boolean;
begin
    // iterate while value is even and higher than 1
    while (((value mod 2) = 0) and (value > 1)) do
        value := value div 2;

    Result := (value = 1);
end;
//--------------------------------------------------------------------------------------------------
class function TQRMathsHelper.RoundUpToNearestPowerOf2(value: TQRUInt32): TQRUInt32;
begin
    if (value = 0) then
    begin
        Result := 1;
        Exit;
    end;

    Dec(value);
    value := value or (value shr 1);
    value := value or (value shr 2);
    value := value or (value shr 4);
    value := value or (value shr 8);
    value := value or (value shr 16);
    Inc(value);

    Result := value;
end;
//--------------------------------------------------------------------------------------------------
class function TQRMathsHelper.GetClosestPowerOf2(value: NativeUInt): NativeUInt;
var
    pos: NativeUInt;
begin
    pos := 0;

    while (value > 0) do
    begin
        Inc(pos);
        value := (value shr 1);
    end;

    Result := Round(Power(2, pos));
end;
//--------------------------------------------------------------------------------------------------
class function TQRMathsHelper.DegToRad(angle: Double): Double;
begin
    Result := ((angle * PI) / 180.0);
end;
//--------------------------------------------------------------------------------------------------
class function TQRMathsHelper.RadToDeg(angle: Double): Double;
begin
    Result := ((angle * 180.0) / PI);
end;
//--------------------------------------------------------------------------------------------------
// TQRMemoryHelper
//--------------------------------------------------------------------------------------------------
class function TQRMemoryHelper.IsSystemBE: Boolean;
type
    // this record is more or less a transcription of an union type in c++
    IEndianness = record
        case Boolean of
            True:  (i: TQRUInt32);
            False: (p: array[1..4] of TQRUInt8);
    end;
var
    bInt: ^IEndianness;
begin
    Result := False;
    bInt   := nil;

    try
        // set a value of 5 inside the endianness structure
        New(bInt);
        bInt^.i := 5;

        // check whether value is on the first or last byte, and thus determine system endianness
        if (bInt^.p[1] = 5) then
            Result := False
        else
        if (bInt^.p[4] = 5) then
            Result := True
        else
            raise Exception.Create('Cannot determine system endianness');
    finally
        if (Assigned(bInt)) then
            Dispose(bInt);
    end;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRMemoryHelper.Swap<T>(var left, right: T);
var
    value: T;
begin
    value := left;
    left  := right;
    right := value;
end;
//--------------------------------------------------------------------------------------------------

end.
