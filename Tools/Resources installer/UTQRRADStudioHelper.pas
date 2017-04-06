// *************************************************************************************************
// * ==> UTQRRADStudioHelper ----------------------------------------------------------------------*
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
 @abstract(@name provides helper functions to detect which RAD Studio version is in use on the local
           computer.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRRADStudioHelper;

interface

uses System.SysUtils,
     System.Win.Registry,
     Vcl.Controls,
     Winapi.CommCtrl,
     Winapi.ShellAPI,
     Winapi.Windows;

type
    {**
     Delphi versions
    }
    TDelphiVersions =
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
    g_DelphiVersionsNames: array[TDelphiVersions] of string =
    (
        'Delphi 4',
        'Delphi 5',
        'Delphi 6',
        'Delphi 7',
        'Delphi 8',
        'BDS 2005',
        'BDS 2006',
        'RAD Studio 2007',
        'RAD Studio 2009',
        'RAD Studio 2010',
        'RAD Studio XE',
        'RAD Studio XE2',
        'RAD Studio XE3',
        'RAD Studio XE4',
        'RAD Studio XE5',
        'RAD Studio XE6',
        'RAD Studio XE7',
        'RAD Studio XE8',
        'RAD Studio 10 Seattle',
        'RAD Studio 10.1 Berlin',
        'RAD Studio 10.2 Tokyo'
    );

    g_DelphiRegPaths: array[TDelphiVersions] of string =
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
     Embarcadero RAD Studio helper
    }
    TQRRADStudioHelper = class
        public
            class function RegKeyExists(const regPath: string; const hRootKey: HKEY): Boolean; static;

            class function RegReadStr(const regPath, regValue: string;
                                                      var str: string;
                                               const hRootKey: HKEY): Boolean; static;

            class procedure ExtractIconFileToImageList(pImageList: TImageList;
                                                   const fileName: string); static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRRADStudioHelper
//--------------------------------------------------------------------------------------------------
class function TQRRADStudioHelper.RegKeyExists(const regPath: string; const hRootKey: HKEY): Boolean;
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
class function TQRRADStudioHelper.RegReadStr(const regPath, regValue: string;
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
class procedure TQRRADStudioHelper.ExtractIconFileToImageList(pImageList: TImageList;
                                                          const fileName: string);
var
    fileInfo: TShFileInfo;
begin
    if (FileExists(filename)) then
    begin
        FillChar(fileInfo, SizeOf(fileInfo), 0);
        SHGetFileInfo(PChar(Filename), 0, fileInfo, SizeOf(fileInfo), SHGFI_ICON or SHGFI_SMALLICON);

        if (fileInfo.hIcon <> 0) then
        begin
            ImageList_AddIcon(pImageList.Handle, fileInfo.hIcon);
            DestroyIcon(fileInfo.hIcon);
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------

end.
