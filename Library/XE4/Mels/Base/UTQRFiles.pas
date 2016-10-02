{**************************************************************************************************
 * ==> UTQRFiles ---------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the basic classes to work with files                        *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRFiles;

interface

uses System.SysUtils,
     System.Classes,
     System.Generics.Collections,
     System.StrUtils,
     UTQRHelpers;

type
    {**
    * Memory file dictionary, allows to associate a file name with a memory buffer, and use it as if
    * it was a HDD file
    }
    TQRMemoryFileDictionary = TDictionary<TFileName, TStream>;

    {**
    * Memory directory, allows to create a structure closest to a dir that contains all files as
    * memory buffers
    *}
    TQRMemoryDir = class
        protected
            m_pFiles:          TQRMemoryFileDictionary;
            m_DeleteOnDestroy: Boolean;

        public
            {**
            * Constructor
            *@param deleteOnDestroy - if true, added memory streams will be deleted while dir
            *                         will be destroyed
            *}
            constructor Create(deleteOnDestroy: Boolean); virtual;

            {**
            * Destructor
            *}
            destructor  Destroy(); override;

            {**
            * Adds memory file
            *@param fileName - memory file name
            *@param pBuffer - memory buffer containing file data
            *@param overwrite - if true, existing file will be overwritten
            *@param caseSensitive - if true, file name will be case sensitive
            *@return true on success, otherwise false
            *}
            function AddFile(const fileName: TFileName;
                                    pBuffer: TStream;
                                  overwrite: Boolean;
                              caseSensitive: Boolean = False): Boolean; virtual;

            {**
            * Gets file
            *@param fileName - memory file name to get
            *@param caseSensitive - if true, file name will be case sensitive
            *@return memory buffer containing file data, nil if not found or on error
            *}
            function GetFile(const fileName: TFileName;
                              caseSensitive: Boolean = False): TStream; virtual;

            {**
            * Checks if file exists
            *@param fileName - file name to check
            *@param caseSensitive - if true, file name will be case sensitive
            *@return true if file exists, otherwise false
            *}
            function FileExists(const fileName: TFileName;
                                 caseSensitive: Boolean = False): Boolean; virtual;
    end;

    {**
    * Base class that provides tools to read and parse generic scripts
    *}
    TQRScript = class
        protected
            {**
            * Parses script
            *@param strings - string list containing loaded script file to parse
            *@return true on success, otherwise false
            *}
            function Parse(const strings: TStringList): Boolean; virtual;

            {**
            * Parses a script line
            *@param line - line to parse
            *@param linbeNb - line number
            *@return ture on success, otherwise false
            *}
            function ParseLine(const line: UnicodeString;
                                   lineNb: NativeUInt): Boolean; virtual;

            {**
            * Called when script line should be parsed
            *@param line - line to parse
            *@param linbeNb - line number
            *@return ture on success, otherwise false
            *}
            function OnParseLine(const line: UnicodeString;
                                     lineNb: NativeUInt): Boolean; virtual; abstract;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Clears script
            *}
            procedure Clear(); virtual; abstract;

            {**
            * Loads script from file
            *@param fileName - script file name
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {**
            * Loads script from buffer
            *@param pBuffer - buffer containing script file to read
            *@param dataLength - buffer length to read
            *@return true on success, otherwise false
            *}
            function Load(const pBuffer: TStream; dataLength: NativeUInt): Boolean; overload; virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMemoryDir
//--------------------------------------------------------------------------------------------------
constructor TQRMemoryDir.Create(deleteOnDestroy: Boolean);
begin
    inherited Create;

    m_pFiles          := TDictionary<TFileName, TStream>.Create;
    m_DeleteOnDestroy := deleteOnDestroy;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMemoryDir.Destroy();
var
    item:   TPair<TFileName, TStream>;
    pValue: TStream;
begin
    // do delete file on destroy?
    if (m_DeleteOnDestroy) then
        // iterate through all registered items
        for item in m_pFiles do
        begin
            pValue := item.Value;
            pValue.Free;
        end;

    m_pFiles.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMemoryDir.AddFile(const fileName: TFileName;
                                     pBuffer: TStream;
                                   overwrite: Boolean;
                               caseSensitive: Boolean): Boolean;
var
    pStreamToDel: TStream;
    name:         TFileName;
begin
    // no buffer to add?
    if (not Assigned(pBuffer)) then
    begin
        Result := False;
        Exit;
    end;

    // is case sensitive?
    if (caseSensitive) then
        name := fileName
    else
        name := LowerCase(fileName);

    // file exists?
    if (m_pFiles.ContainsKey(name)) then
    begin
        // file cannot be overwritten?
        if (not overwrite) then
        begin
            Result := False;
            Exit;
        end;

        // do delete file content?
        if (m_DeleteOnDestroy) then
        begin
            // get previous memory stream to delete
            pStreamToDel := m_pFiles.Items[name];

            // delete memory stream
            pStreamToDel.Free;

            // delete previous item from cache
            m_pFiles.Remove(name);
        end;
    end;

    // add file to dir list
    m_pFiles.Add(name, pBuffer);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMemoryDir.GetFile(const fileName: TFileName; caseSensitive: Boolean): TStream;
var
    name: TFileName;
begin
    // is case sensitive?
    if (caseSensitive) then
        name := fileName
    else
        name := LowerCase(fileName);

    // file not exists?
    if (not m_pFiles.ContainsKey(name)) then
    begin
        Result := nil;
        Exit;
    end;

    // get stream matching with file
    Result := m_pFiles.Items[name];
end;
//--------------------------------------------------------------------------------------------------
function TQRMemoryDir.FileExists(const fileName: TFileName; caseSensitive: Boolean): Boolean;
begin
    // is case sensitive?
    if (caseSensitive) then
        Result := m_pFiles.ContainsKey(fileName)
    else
        Result := m_pFiles.ContainsKey(LowerCase(fileName));
end;
//--------------------------------------------------------------------------------------------------
// TQRScript
//--------------------------------------------------------------------------------------------------
constructor TQRScript.Create();
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRScript.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRScript.Parse(const strings: TStringList): Boolean;
var
    i, lineCount: NativeUInt;
begin
    // clear all previous data before parsing new
    Clear();

    // get line count
    lineCount := strings.Count;

    // no line to parse?
    if (lineCount = 0) then
    begin
        Result := True;
        Exit;
    end;

    // iterate through lines to parse
    for i := 0 to lineCount - 1 do
        // parse line
        if (not ParseLine(strings[i], i)) then
        begin
            Result := False;
            Exit;
        end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRScript.ParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean;
begin
    Result := OnParseLine(line, lineNb);
end;
//--------------------------------------------------------------------------------------------------
function TQRScript.Load(const fileName: TFileName): Boolean;
var
    fileBuffer: TFileStream;
begin
    // script file exists?
    if (not FileExists(fileName)) then
    begin
        Result := False;
        Exit;
    end;

    fileBuffer := nil;

    try
        // open script file
        fileBuffer := TFileStream.Create(fileName, fmOpenRead);

        // succeeded?
        if (fileBuffer.Size = 0) then
        begin
            Result := False;
            Exit;
        end;

        fileBuffer.Seek(0, soBeginning);

        // load script from file buffer
        Result := Load(fileBuffer, fileBuffer.Size);
    finally
        fileBuffer.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRScript.Load(const pBuffer: TStream; dataLength: NativeUInt): Boolean;
var
    strings: TStringList;
begin
    strings := nil;

    try
        // create a new string container to contain script file content
        strings := TStringList.Create;

        // load script file
        strings.LoadFromStream(pBuffer);

        // parse it
        Result := Parse(strings);
    finally
        strings.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
