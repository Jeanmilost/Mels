// *************************************************************************************************
// * ==> UTQRPlayer -------------------------------------------------------------------------------*
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
 @abstract(@name provides an interface to implement a sound or music player.)
 @image(Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRPlayer;

interface

type
    {$REGION 'Documentation'}
    {**
     Interface for sound or music player
    }
    {$ENDREGION}
    IQRPlayer = interface['{BC6EE76C-37C9-4565-947C-90F67E07CC83}']
        {$REGION 'Documentation'}
        {**
         Plays sound
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        function Play: Boolean;

        {$REGION 'Documentation'}
        {**
         Pauses sound
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        function Pause: Boolean;

        {$REGION 'Documentation'}
        {**
         Stops sound
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        function Stop: Boolean;

        {$REGION 'Documentation'}
        {**
         Checks if playback is already playing
         @return(@true if playback is already playing, otherwise @false)
        }
        {$ENDREGION}
        function IsPlaying: Boolean;

        {$REGION 'Documentation'}
        {**
         Changes volume
         @param(value volume value between 0.0f (lowest) and 1.0f (highest))
         @return(@true on success, otherwise @false)
        }
        {$ENDREGION}
        function ChangeVolume(const value: Single): Boolean;

        {$REGION 'Documentation'}
        {**
         Loops the music
         @param(value whether or not sound should loop)
        }
        {$ENDREGION}
        procedure Loop(value: Boolean);
    end;

implementation

end.
