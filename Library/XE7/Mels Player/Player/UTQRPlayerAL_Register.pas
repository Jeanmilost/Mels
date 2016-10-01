{**************************************************************************************************
 * ==> UTQRPlayerAL_Register ---------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module allows to register components in RAD studio tool palette             *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRPlayerAL_Register;

interface

uses System.Classes,
     UTQRPlayerAL;

{**
* Main register procedure
*}
procedure Register;

implementation
//--------------------------------------------------------------------------------------------------
// Main register procedure
//--------------------------------------------------------------------------------------------------
procedure Register;
begin
    // register all components to show in the designer toolbar
    RegisterComponents('Mels Player', [TQRPlayerAL]);
end;
//--------------------------------------------------------------------------------------------------

end.
