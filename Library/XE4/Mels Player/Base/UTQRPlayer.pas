{**************************************************************************************************
 * ==> UTQRPlayer --------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides an interface to implement a sound or music player.          *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRPlayer;

interface

type
    {**
    * Interface for sound or music player
    *}
    IQRPlayer = interface['{BC6EE76C-37C9-4565-947C-90F67E07CC83}']
        {**
        * Plays sound
        *@return true on success, otherwise false
        *}
        function Play: Boolean;

        {**
        * Pauses sound
        *@return true on success, otherwise false
        *}
        function Pause: Boolean;

        {**
        * Stops sound
        *@return true on success, otherwise false
        *}
        function Stop: Boolean;

        {**
        * Checks if playback is already playing
        *@return true if playback is already playing, otherwise false
        *}
        function IsPlaying: Boolean;

        {**
        * Changes volume
        *@param value - volume value between 0.0f (lowest) and 1.0f (highest)
        *@return true on success, otherwise false
        *}
        function ChangeVolume(const value: Single): Boolean;

        {**
        * Loops the music
        *@param value - whether or not sound should loop
        *}
        procedure Loop(value: Boolean);
    end;

implementation

end.
