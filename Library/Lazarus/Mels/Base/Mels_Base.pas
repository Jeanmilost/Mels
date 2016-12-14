{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Mels_Base;

interface

uses
    UTQR3D, UTQRCollision, UTQRCommon, UTQRDesignPatterns, UTQRFiles, 
    UTQRGeometry, UTQRGraphics, UTQRHelpers, UTQRCache, Generics.Collections, 
    Generics.Defaults, Generics.Hashes, Generics.Helpers, 
    Generics.MemoryExpanders, Generics.Strings, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('Mels_Base', @Register);
end.
