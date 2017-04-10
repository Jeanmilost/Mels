// *************************************************************************************************
// * ==> UTQRRadStudioHelper ----------------------------------------------------------------------*
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
 @abstract(@name provides helper functions to detect which Rad Studio version is in use on the local
           computer.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRRadStudioHelper;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     System.Win.Registry,
     Vcl.Controls,
     Winapi.CommCtrl,
     Winapi.ShellAPI,
     Winapi.Windows;

type
    {**
     Rad Studio versions
    }
    EQRRadStudioVersion =
    (
        Delphi4,
        Delphi5,
        Delphi6,
        Delphi7,
        Delphi8,
        Delphi2005,
        Delphi2006,
        Delphi2007,
        Delphi2009,
        Delphi2010,
        DelphiXE,
        DelphiXE2,
        DelphiXE3,
        DelphiXE4,
        DelphiXE5,
        DelphiXE6,
        DelphiXE7,
        DelphiXE8,
        Delphi10_Seattle,
        Delphi10_1_Berlin,
        Delphi10_2_Tokyo
    );

const
    {**
     Rad Studio human readable names
    }
    g_RadStudioName: array[EQRRadStudioVersion] of string =
    (
        'Delphi 4',
        'Delphi 5',
        'Delphi 6',
        'Delphi 7',
        'Delphi 8',
        'BDS 2005',
        'BDS 2006',
        'Rad Studio 2007',
        'Rad Studio 2009',
        'Rad Studio 2010',
        'Rad Studio XE',
        'Rad Studio XE2',
        'Rad Studio XE3',
        'Rad Studio XE4',
        'Rad Studio XE5',
        'Rad Studio XE6',
        'Rad Studio XE7',
        'Rad Studio XE8',
        'Rad Studio 10 Seattle',
        'Rad Studio 10.1 Berlin',
        'Rad Studio 10.2 Tokyo'
    );

    {**
     Rad Studio registry paths
    }
    g_RadStudioRegPath: array[EQRRadStudioVersion] of string =
    (
        '\Software\Borland\Delphi\4.0',
        '\Software\Borland\Delphi\5.0',
        '\Software\Borland\Delphi\6.0',
        '\Software\Borland\Delphi\7.0',
        '\Software\Borland\BDS\2.0',
        '\Software\Borland\BDS\3.0',
        '\Software\Borland\BDS\4.0',
        '\Software\Borland\BDS\5.0',
        '\Software\CodeGear\BDS\6.0',
        '\Software\CodeGear\BDS\7.0',
        '\Software\Embarcadero\BDS\8.0',
        '\Software\Embarcadero\BDS\9.0',
        '\Software\Embarcadero\BDS\10.0',
        '\Software\Embarcadero\BDS\11.0',
        '\Software\Embarcadero\BDS\12.0',
        '\Software\Embarcadero\BDS\14.0',
        '\Software\Embarcadero\BDS\15.0',
        '\Software\Embarcadero\BDS\16.0',
        '\Software\Embarcadero\BDS\17.0',
        '\Software\Embarcadero\BDS\18.0',
        '\Software\Embarcadero\BDS\19.0'
    );

type
    {**
     Embarcadero Rad Studio helper
    }
    TQRRadStudioHelper = class
        public type
            {**
             Installed version info
            }
            IInstalledVersionInfo = record
                m_Version: EQRRadStudioVersion;
                m_BinName: TFileName;
            end;

            {**
             List of installed versions
            }
            IQRInstalledVersions = TList<IInstalledVersionInfo>;

            {**
             Key-value dictionary to contain several Rad studio properties
            }
            IQRKeyValueDictionary = TDictionary<UnicodeString, UnicodeString>;

            {**
             Key-value pair
            }
            IQRKeyValuePair = TPair<UnicodeString, UnicodeString>;

        private
            {**
             Checks in the registry if a key exists
             @param(regPath Key path in the registry)
             @param(hRootKey The registry root key)
             @return(@true if key exists in the registry, otherwise @false)
            }
            class function RegKeyExists(const regPath: string; const hRootKey: HKEY): Boolean; static;

            {**
             Read key content as string value
             @param(regPath Key path in the registry)
             @param(regValue Key name in the registry)
             @param(str @bold([out]) Key content as string value)
             @param(hRootKey The registry root key)
             @return(@true on success, otherwise @false)
            }
            class function RegReadStr(const regPath, regValue: string;
                                                      var str: string;
                                               const hRootKey: HKEY): Boolean; static;

        public
            {**
             Get the installed version info list
             @param(pVersions Installed version list to populate, populated list on function ends)
            }
            class procedure GetInstalledVersions(pVersions: IQRInstalledVersions); static;

            {**
             Get the Rad Studio icons in an image list
             @param(fileName Rad Studio IDE binary file name and path)
             @param(pImageList Image list to populate, populated list on function ends)
            }
            class procedure ExtractIconFileToImageList(const fileName: string;
                                                           pImageList: TImageList); static;

            {**
             Get a list of key-value pairs containing the paths used by the Rad Studio IDE
             @param(installDir Rad Studio installation directory)
             @param(pPaths Key-value pair list to populate, populated list on function ends)
            }
            class procedure GetPaths(const installDir: UnicodeString;
                                               pPaths: IQRKeyValueDictionary); static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRRadStudioHelper
//--------------------------------------------------------------------------------------------------
class function TQRRadStudioHelper.RegKeyExists(const regPath: string; const hRootKey: HKEY): Boolean;
var
    pReg: TRegistry;
begin
    try
        pReg := TRegistry.Create;

        try
            pReg.RootKey := hRootKey;
            Result       := pReg.KeyExists(regPath);
        finally
            pReg.Free;
        end;
    except
        Result := False;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRRadStudioHelper.RegReadStr(const regPath, regValue: string;
                                                             var str: string;
                                                      const hRootKey: HKEY): Boolean;
var
    pReg: TRegistry;
begin
    try
        pReg := TRegistry.Create;

        try
            pReg.RootKey := hRootKey;
            Result       := pReg.OpenKey(regPath, True);

            if (Result) then
                str := pReg.ReadString(regValue);
        finally
            pReg.Free;
        end;
    except
        Result := False;
    end;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRRadStudioHelper.GetInstalledVersions(pVersions: IQRInstalledVersions);
var
    versionInfo: IInstalledVersionInfo;
    version:     EQRRadStudioVersion;
    fileName:    string;
    found:       Boolean;
begin
    if (not Assigned(pVersions)) then
        Exit;

    // iterate through Delphi versions to detect
    for version := Low(EQRRadStudioVersion) to High(EQRRadStudioVersion) do
    begin
        // check if matching key exists in HKEY_CURRENT_USER
        found := TQRRadStudioHelper.RegKeyExists(g_RadStudioRegPath[version], HKEY_CURRENT_USER);

        // found it?
        if (found) then
            // get the exe file name and check if the file exists
            found := TQRRadStudioHelper.RegReadStr(g_RadStudioRegPath[version],
                                                   'App',
                                                   fileName,
                                                   HKEY_CURRENT_USER) and FileExists(fileName);

        // still not found?
        if (not found) then
        begin
            // check if matching key exists in HKEY_LOCAL_MACHINE
            found := TQRRadStudioHelper.RegKeyExists(g_RadStudioRegPath[version], HKEY_LOCAL_MACHINE);

            // found it?
            if (found) then
                // get the exe file name and check if the file exists
                found := TQRRadStudioHelper.RegReadStr(g_RadStudioRegPath[version],
                                                       'App',
                                                       fileName,
                                                       HKEY_LOCAL_MACHINE) and FileExists(fileName);
        end;

        // still not found?
        if (not found) then
            continue;

        versionInfo           := Default(IInstalledVersionInfo);
        versionInfo.m_Version := version;
        versionInfo.m_BinName := fileName;

        pVersions.Add(versionInfo);
    end;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRRadStudioHelper.ExtractIconFileToImageList(const fileName: string;
                                                                  pImageList: TImageList);
var
    fileInfo: TShFileInfo;
begin
    if (not FileExists(filename)) then
        Exit;

    // get the file info struct. This struct may contain the icon image to get
    FillChar(fileInfo, SizeOf(fileInfo), 0);
    SHGetFileInfo(PChar(Filename), 0, fileInfo, SizeOf(fileInfo), SHGFI_ICON or SHGFI_SMALLICON);

    // found icon?
    if (fileInfo.hIcon <> 0) then
    begin
        // extract icon from binary file
        ImageList_AddIcon(pImageList.Handle, fileInfo.hIcon);
        DestroyIcon(fileInfo.hIcon);
    end;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRRadStudioHelper.GetPaths(const installDir: UnicodeString;
                                                      pPaths: IQRKeyValueDictionary);
var
    rsvars:                             TFileName;
    item, line, keyValPair, key, value: UnicodeString;
    separatorPos:                       NativeInt;
    pStrings:                           TStringList;
begin
    if (not Assigned(pPaths)) then
        Exit;

    // get the rsvars file name
    rsvars   := IncludeTrailingPathDelimiter(installDir) + 'rsvars.bat';
    pStrings := nil;

    try
        // create a string list container and open the rsvars file
        pStrings := TStringList.Create;
        pStrings.LoadFromFile(rsvars);

        // iterate through rsvars lines
        for item in pStrings do
        begin
            // get upper-case line
            line := UpperCase(item);

            // check if the line begins with a @SET instruction
            if (Pos('@SET ', line) <> 1) then
                Continue;

            // extract the key-value pair
            keyValPair := item.Substring(5, Length(item) - 5);

            // search for the separator
            separatorPos := Pos('=', keyValPair);

            // no separator found?
            if (separatorPos = 0) then
                continue;

            // extract the key and value
            key   := keyValPair.Substring(0, separatorPos - 1);
            value := keyValPair.Substring(separatorPos, Length(keyValPair) - separatorPos);

            pPaths.Add(key, value);
        end;
    finally
        pStrings.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
