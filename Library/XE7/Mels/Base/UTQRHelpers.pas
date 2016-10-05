{**************************************************************************************************
 * ==> UTQRHelpers -------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This unit provides helpers to support some common low level tasks that Delphi    *
 *               classes don't care.                                                              *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRHelpers;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     UTQRCommon;

const
    CQR_Win_Dir_Delimiter = Chr($5C); // on Windows system, dir delimiter is a '\'
    CQR_OSx_Dir_Delimiter = Chr($2F); // on OSX/iOS systems, dir delimiter is a '/'
    CQR_Zip_Dir_Delimiter = Chr($2F); // inside ZIP packages, dir delimiter is a '/'

    {$IF DEFINED(MSWINDOWS)}
        CQR_Dir_Delimiter = CQR_Win_Dir_Delimiter;
    {$ELSE IF DEFINED(MACOS) or DEFINED(IOS)}
        CQR_Dir_Delimiter = CQR_OSx_Dir_Delimiter;
    {$ELSE}
        {$MESSAGE FATAL 'Unknown platform type'}
    {$IFEND}
type
    TQRByteArray     = array of Byte;
    TQRAnsiCharArray = array of AnsiChar;
    TQRCharArray     = Array of Char;

    {**
    * Some helper functions to manipulate strings
    *}
    TQRStringHelper = Record
        public
            {**
            * Converts array of ansi char to string
            *@param chars - array of ansi chars to convert
            *@return string
            *}
            class function AnsiCharArrayToStr(const chars: TQRAnsiCharArray): String; static;

            {**
            * Converts string to array of bytes
            *@param str - string to convert
            *@return array of bytes
            *}
            class function StrToByte(const str: String): TQRByteArray; static;

            {**
            * Converts array of bytes to string
            *@param bytes - array of bytes to convert
            *@return string
            *}
            class function ByteToStr(const bytes: TQRByteArray): String; static;

            {**
            * Checks if a digit contains only chars that can be converted to number
            *@param digit - digit to check
            *@param strict - if true, only chars from '0' to '9' will be accepted, see note below
            *@return true if digit contains only chars that can be converted to number, otherwise false
            *@note If strict mode is et to false, math symbols as e.g. '.' or '-' will also be accepted
            *      as valid numeric chars. This may be useful when IsNumeric() is used e.g. to determine
            *      if a string can be converted to number
            *}
            class function IsNumeric(digit: AnsiChar; isStrict: Boolean): Boolean; overload; static;
            class function IsNumeric(digit: WideChar; isStrict: Boolean): Boolean; overload; static;

            {**
            * Searches the last position of a sub-string inside a string
            *@param subStr - sub-string to find
            *@param str - string containing the sub-string to find
            *@return last sub-string position inside the string, 0 if not found
            *}
            class function LastPos(const subStr, str: UnicodeString): Integer; static;

            {**
            * Reverses a string content
            *@param str - string to reverse
            *@return reversed string
            *}
            class function Reverse(const str: UnicodeString): UnicodeString; static;
    end;

    PQRStringHelper = ^TQRStringHelper;

    {**
    * Some helper functions to manipulate files
    *}
    TQRFileHelper = record
        public
            {**
            * Extracts file name
            *@param fileName - file name to extract from
            *@param delimiter - dir delimiter to use
            *@return file name without extension, empty string if not found
            *}
            class function ExtractFileName(const fileName: TFileName;
                                          const delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;

            {**
            * Extracts file name without extension
            *@param fileName - file name to extract from
            *@param delimiter - dir delimiter to use
            *@return file name without extension, empty string if not found
            *}
            class function ExtractFileNameNoExt(const fileName: TFileName;
                                               const delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;

            {**
            * Saves byte array to file
            *@param pBytes - bytes to save
            *}
            class procedure SaveBytesToFile(const fileName: TFileName;
                                                var pBytes: TQRByteArray); static;

            {**
            * Appends delimiter at dir end
            *@param dirName - directory name to append to
            *@return appended dir name
            *@note If delimiter already exists at end, it will not be appended again
            *}
            class function AppendDelimiter(const dirName: UnicodeString;
                                               delimiter: Char = CQR_Dir_Delimiter): UnicodeString; static;
    end;

    PQRFileHelper = ^TQRFileHelper;

    {**
    * Some helper functions for mathematics
    *}
    TQRMathsHelper = record
        public
            {**
            * Checks if a value is a power of 2
            *@param x - value to check
            *@return true if value is a power of two, otherwise false
            *}
            class function IsPowerOfTwo(value: NativeUInt): Boolean; static;

            {**
            * Rounds up to the nearest power of 2
            *@param value - value to round up
            *@return rounded up power of 2
            *}
            class function RoundUpToNearestPowerOf2(value: TQRUInt32): TQRUInt32; static;

            {**
            * Gets the closest power of 2 from a value
            *@aram value - value
            *@return closest power of 2
            *}
            class function GetClosestPowerOf2(value: NativeUInt): NativeUInt; static;

            {**
            * Converts degrees to radians
            *@param angle - angle in degrees
            *@return angle in radians
            *}
            class function DegToRad(angle: Double): Double; static;

            {**
            * Converts radians to degrees
            *@param angle - angle in radians
            *@return angle in degrees
            *}
            class function RadToDeg(angle: Double): Double; static;
    end;

    {**
    * Some helper functions to manipulate memory
    *}
    TQRMemoryHelper = record
        public
            {**
            * Checks if system on which program is executed is big endian
            *@return true if system on which program is executed is big endian, false if little endian
            *}
            class function IsSystemBE(): Boolean; static;

            {**
            * Swaps content of 2 variables
            *@param[in, out] a - first variable to swap
            *@param[in, out] b - second variable to swap
            *}
            class procedure Swap<T>(var left, right: T); static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRStringHelper
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.AnsiCharArrayToStr(const chars: TQRAnsiCharArray): String;
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
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.StrToByte(const str: String): TQRByteArray;
var
    i:      NativeInt;
    strLen: NativeUInt;
begin
    // get string length to convert
    strLen := Length(str);

    // initialize memory for array of bytes
    SetLength(Result, strLen);

    // iterate through string chars and convert them to byte
    for i := 0 to strLen - 1 do
    begin
        Result[i] := Ord(str[i + 1]);
        Dec(Result[i], 48);
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRStringHelper.ByteToStr(const bytes: TQRByteArray): String;
var
    i:         NativeInt;
    strLen:    NativeUInt;
    s:         String;
    character: Char;
begin
    s      := '';
    strLen := Length(bytes);

    // iterate through bytes and convert them to chars, then add them to string
    for i := strLen - 1 Downto 0 do
    begin
        character := Chr(bytes[i] + 48);
        s         := character + s;
    end;

    Result := s;
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
    if (dirName.IsEmpty) then
    begin
        Result := UnicodeString.Create(delimiter, 1);
        Exit;
    end;

    // dir name already contains delimiter at end?
    if (dirName.Chars[Length(dirName) - 1] = delimiter) then
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
class function TQRMemoryHelper.IsSystemBE(): Boolean;
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
