// *************************************************************************************************
// * ==> UTQRFiles --------------------------------------------------------------------------------*
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
 @abstract(@name provides some additional features to facilitate the work with files.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRFiles;

interface

uses System.SysUtils,
     System.Classes,
     System.Generics.Collections,
     System.StrUtils,
     UTQRHelpers;

type
    {$REGION 'Documentation'}
    {**
     Memory file dictionary, allows to associate a file name with a memory buffer, and use it as if
     it was a HDD file
    }
    {$ENDREGION}
    TQRMemoryFileDictionary = TDictionary<TFileName, TStream>;

    {$REGION 'Documentation'}
    {**
     Memory directory, allows to create a structure closest to a dir that contains all files as
     memory buffers
    }
    {$ENDREGION}
    TQRMemoryDir = class
        private
            m_pFiles:          TQRMemoryFileDictionary;
            m_DeleteOnDestroy: Boolean;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(deleteOnDestroy If @true, added memory streams will be deleted while the dir
                                    will be destroyed)
            }
            {$ENDREGION}
            constructor Create(deleteOnDestroy: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Adds memory file
             @param(fileName Memory file name)
             @param(pBuffer Memory buffer containing file data)
             @param(overwrite If t@rue, existing file will be overwritten)
             @param(caseSensitive If @true, file name will be case sensitive)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function AddFile(const fileName: TFileName;
                                    pBuffer: TStream;
                                  overwrite: Boolean;
                              caseSensitive: Boolean = False): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Gets file
             @param(fileName Memory file name to get)
             @param(caseSensitive If @true, file name will be case sensitive)
             @return(Memory buffer containing file data, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetFile(const fileName: TFileName;
                              caseSensitive: Boolean = False): TStream; virtual;

            {$REGION 'Documentation'}
            {**
             Checks if file exists
             @param(fileName File name to check)
             @param(caseSensitive If @true, file name will be case sensitive)
             @return(@true if file exists, otherwise @false)
            }
            {$ENDREGION}
            function FileExists(const fileName: TFileName;
                                 caseSensitive: Boolean = False): Boolean; virtual;
    end;

    {$REGION 'Documentation'}
    {**
     Base class that provides tools to read and parse generic scripts
    }
    {$ENDREGION}
    TQRScript = class
        protected
            {$REGION 'Documentation'}
            {**
             Parses script
             @param(lines String list containing loaded script file lines to parse)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Parse(const lines: TStringList): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Parses a script line
             @param(line Line to parse)
             @param(linbeNb Line number)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function ParseLine(const line: UnicodeString;
                                   lineNb: NativeUInt): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when script line should be parsed
             @param(line Line to parse)
             @param(linbeNb Line number)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function OnParseLine(const line: UnicodeString;
                                     lineNb: NativeUInt): Boolean; virtual; abstract;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Clears script
            }
            {$ENDREGION}
            procedure Clear; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Loads script from file
             @param(fileName Script file name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads script from buffer
             @param(pBuffer Buffer containing script file to read)
             @param(dataLength Buffer length to read)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
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
destructor TQRMemoryDir.Destroy;
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
        Exit(False);

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
            Exit(False);

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
        Exit(nil);

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
constructor TQRScript.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRScript.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRScript.Parse(const lines: TStringList): Boolean;
var
    line:  UnicodeString;
    index: NativeUInt;
begin
    // clear all previous data before parsing new
    Clear;

    index := 0;

    // iterate through lines to parse
    for line in lines do
    begin
        // parse line
        if (not ParseLine(line, index)) then
            Exit(False);

        Inc(Index);
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
        Exit(False);

    fileBuffer := nil;

    try
        // open script file
        fileBuffer := TFileStream.Create(fileName, fmOpenRead);

        // succeeded?
        if (fileBuffer.Size = 0) then
            Exit(False);

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
