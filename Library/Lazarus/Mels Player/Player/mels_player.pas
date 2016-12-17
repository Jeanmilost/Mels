{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Mels_Player;

interface

uses
    UTQRPlayerAL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UTQRPlayerAL', @UTQRPlayerAL.Register);
end;

initialization
  RegisterPackage('Mels_Player', @Register);
end.
