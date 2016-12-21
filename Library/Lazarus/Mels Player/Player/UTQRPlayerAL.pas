// *************************************************************************************************
// * ==> UTQRPlayerAL -----------------------------------------------------------------------------*
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
 @abstract(@name is a WAV sound player component using OpenAL.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRPlayerAL;

interface

uses Classes,
     SysUtils,
     openal,
     UTQRPlayer;

type
    {$REGION 'Documentation'}
    {**
     Called when music player is playing
     @param(pSender event sender)
    }
    {$ENDREGION}
    TQROnPlayerPlayEvent = procedure (pSender: TObject) of object;

    {$REGION 'Documentation'}
    {**
     Called when music player is pausing
     @param(pSender event sender)
    }
    {$ENDREGION}
    TQROnPlayerPauseEvent = procedure (pSender: TObject) of object;

    {$REGION 'Documentation'}
    {**
     Called when music player is stopping
     @param(pSender event sender)
    }
    {$ENDREGION}
    TQROnPlayerStopEvent = procedure (pSender: TObject) of object;

    {$REGION 'Documentation'}
    {**
     Called when volume is changed on music player
     @param(pSender event sender)
     @param(value new volume value between 0.0f (lowest) and 1.0f (highest))
    }
    {$ENDREGION}
    TQROnPlayerChangeVolumeEvent = procedure (pSender: TObject; value: Single) of object;

    {$REGION 'Documentation'}
    {**
     Simple player using OpenAL
     @br @bold(NOTE) Here is the OpenAL official website: http://www.openal.org/
    }
    {$ENDREGION}
    TQRPlayerAL = class(TComponent, IQRPlayer)
        private
            m_pDevice:         TALCdevice;
            m_pContext:        TALCcontext;
            m_ID:              TALuint;
            m_BufferID:        TALuint;
            m_Sampling:        NativeUInt;
            m_WavName:         TFileName;
            m_pWav:            TMemoryStream;
            m_Allowed:         Boolean;
            m_FOnPlay:         TQROnPlayerPlayEvent;
            m_fOnPause:        TQROnPlayerPauseEvent;
            m_fOnStop:         TQROnPlayerStopEvent;
            m_fOnChangeVolume: TQROnPlayerChangeVolumeEvent;

            {$REGION 'Documentation'}
            {**
             Release OpenAL resources
            }
            {$ENDREGION}
            procedure Release;

        protected const
            {$REGION 'Documentation'}
            {**
             Error identifier
            }
            {$ENDREGION}
            CQR_ErrorID: NativeUInt = $FFFFFFFF;

        protected
            {$REGION 'Documentation'}
            {**
             Sets sampling rate
             @param(sampling sampling rate)
            }
            {$ENDREGION}
            procedure SetSampling(sampling: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets wave file name to load
             @param(FileName wav file name)
            }
            {$ENDREGION}
            procedure SetWavName(FileName: TFileName); virtual;

            {$REGION 'Documentation'}
            {**
             Declares properties that will deal with DFM files
             @param(pFiler DFM file manager)
            }
            {$ENDREGION}
            procedure DefineProperties(pFiler: TFiler); override;

            {$REGION 'Documentation'}
            {**
             Reads WAV content from DFM file
             @param(pStream stream containing DFM data)
            }
            {$ENDREGION}
            procedure ReadWav(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Writes WAV content to DFM file
             @param(pStream DFM stream in which package should be written)
            }
            {$ENDREGION}
            procedure WriteWav(pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Called after control was fully loaded from DFM stream
            }
            {$ENDREGION}
            procedure Loaded; override;

            {$REGION 'Documentation'}
            {**
             Loads the wav file from memory stream
            }
            {$ENDREGION}
            procedure LoadWav; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pOwner component owner)
            }
            {$ENDREGION}
            constructor Create(pOwner: TComponent); override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Opens sound file
             @param(pFileBuffer file buffer)
             @param(FileSize file size)
             @param(sampling sampling rate, default value is 44'100 Hertz)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Buffer content must be wav format or uncompressed
            }
            {$ENDREGION}
            function Open(const pFileBuffer: PByte;
                                   FileSize: NativeUInt;
                                   sampling: NativeUInt = 44100): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Plays sound
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Play: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Pauses sound
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Pause: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Stops sound
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Stop: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Checks if playback is already playing
             @return(@true if playback is already playing, otherwise @false)
            }
            {$ENDREGION}
            function IsPlaying: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Changes volume
             @param(value volume value between 0.0f (lowest) and 1.0f (highest))
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function ChangeVolume(const value: Single): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Loops the music
             @param(value whether or not sound should loop)
            }
            {$ENDREGION}
            procedure Loop(value: Boolean); virtual;

        // properties
        published
            {$REGION 'Documentation'}
            {**
             Gets or sets the sampling rate, default value is 44'100 Hertz
            }
            {$ENDREGION}
            property Sampling: NativeUInt read m_Sampling write SetSampling default 44100;

            {$REGION 'Documentation'}
            {**
             Gets or sets the WAV file name
            }
            {$ENDREGION}
            property WavName: TFileName read m_WavName write SetWavName;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnPlay event
            }
            {$ENDREGION}
            property OnPlay: TQROnPlayerPlayEvent read m_fOnPlay write m_fOnPlay;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnPause event
            }
            {$ENDREGION}
            property OnPause: TQROnPlayerPauseEvent read m_fOnPause write m_fOnPause;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnStop event
            }
            {$ENDREGION}
            property OnStop: TQROnPlayerStopEvent read m_fOnStop write m_fOnStop;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnChangeVolume event
            }
            {$ENDREGION}
            property OnChangeVolume: TQROnPlayerChangeVolumeEvent read m_fOnChangeVolume write m_fOnChangeVolume;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R UTQRPlayerAL.rc}
//--------------------------------------------------------------------------------------------------
// TQRPlayerAL
//--------------------------------------------------------------------------------------------------
constructor TQRPlayerAL.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pDevice         := nil;
    m_pContext        := nil;
    m_ID              := CQR_ErrorID;
    m_BufferID        := CQR_ErrorID;
    m_Sampling        := 44100;
    m_pWav            := TMemoryStream.Create;
    m_Allowed         := InitOpenAL and Assigned(@alcOpenDevice);
    m_fOnPlay         := nil;
    m_fOnPause        := nil;
    m_fOnStop         := nil;
    m_fOnChangeVolume := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRPlayerAL.Destroy;
begin
    // stop music before closing it
    if (IsPlaying) then
        Stop;

    Release;

    // clear memory
    m_pWav.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.Release;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
        Exit;

    // delete sources
    if (m_ID <> CQR_ErrorID) then
    begin
        alDeleteSources(1, @m_ID);
        m_ID := CQR_ErrorID;
    end;

    // delete buffers
    if (m_BufferID <> CQR_ErrorID) then
    begin
        alDeleteBuffers(1, @m_BufferID);
        m_BufferID := CQR_ErrorID;
    end;

    // destroy context
    if (Assigned(m_pContext)) then
    begin
        alcMakeContextCurrent(nil);
        alcDestroyContext(m_pContext);
        m_pContext := nil;
    end;

    // close device
    if (Assigned(m_pDevice)) then
    begin
        alcCloseDevice(m_pDevice);
        m_pDevice := nil;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.SetSampling(sampling: NativeUInt);
begin
    // nothing to do?
    if (m_Sampling = sampling) then
        Exit;

    // stop music in case it was playing
    if (IsPlaying) then
        Stop;

    Release;

    m_Sampling := sampling;

    LoadWav;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.SetWavName(fileName: TFileName);
var
    fileStream: TFileStream;
begin
    // no changes?
    if (fileName = m_WavName) then
        Exit;

    // stop music in case it was playing
    if (IsPlaying) then
        Stop;

    Release;

    // if component is currently loading, just update the name, the wav will be loaded later
    if (csLoading in ComponentState) then
    begin
        m_WavName := fileName;
        Exit;
    end;

    // previous wav was loaded?
    if (m_pWav.Size > 0) then
        // clear it
        m_pWav.Clear;

    // file name isn't empty, file exists and is a wav file?
    if ((Length(fileName) = 0)     or
        (not FileExists(fileName)) or
        (LowerCase(ExtractFileExt(fileName)) <> '.wav'))
    then
    begin
        // clear previously loaded model
        m_WavName := '';
        Exit;
    end;

    // update name and open file stream
    m_WavName  := fileName;
    fileStream := TFileStream.Create(m_WavName, fmOpenRead);

    try
        // copy package file content to memory
        m_pWav.CopyFrom(fileStream, fileStream.Size);
    finally
        fileStream.Free;
    end;

    LoadWav;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.DefineProperties(pFiler: TFiler);
    function DoWriteWav: Boolean;
    begin
        if Assigned(pFiler.Ancestor) then
            Result := not (pFiler.Ancestor is TQRPlayerAL)
        else
            Result := m_pWav.Size > 0;
    end;
begin
    inherited DefineProperties(pFiler);

    // register the properties that will load and save a binary data in DFM files
    pFiler.DefineBinaryProperty('Package', @ReadWav, @WriteWav, DoWriteWav);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.ReadWav(pStream: TStream);
begin
    // previous package was loaded?
    if (m_pWav.Size > 0) then
        // clear it
        m_pWav.Clear;

    // read model package from DFM stream
    m_pWav.CopyFrom(pStream, pStream.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.WriteWav(pStream: TStream);
begin
    // reset stream position to start
    m_pWav.Position := 0;

    // write model package to DFM stream
    pStream.CopyFrom(m_pWav, m_pWav.Size);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.Loaded;
begin
    inherited Loaded;

    LoadWav;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.LoadWav;
begin
    // is component in desing time?
    if (csDesigning in ComponentState) then
        Exit;

    // no data to load?
    if (m_pWav.Size = 0) then
        Exit;

    m_pWav.Position := 0;

    // open wav file
    Open(PByte(m_pWav.Memory), m_pWav.Size, m_Sampling);
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.Open(const pFileBuffer: PByte;
                                   fileSize: NativeUInt;
                                   sampling: NativeUInt): Boolean;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    Release;

    // no sound file to load?
    if (not(Assigned(pFileBuffer)) or (fileSize = 0)) then
    begin
        Result := False;
        Exit;
    end;

    try
        // select the "preferred device"
        m_pDevice := alcOpenDevice(nil);

        // found it?
        if (not Assigned(m_pDevice)) then
        begin
            Result := False;
            Exit;
        end;

        // use the device to make a context
        m_pContext := alcCreateContext(m_pDevice, nil);

        // found it?
        if (not Assigned(m_pContext)) then
        begin
            Release;
            Result := False;
            Exit;
        end;

        // set context to the currently active one
        alcMakeContextCurrent(m_pContext);

        // grab a buffer ID from openAL
        alGenBuffers(1, @m_BufferID);

        // jam the audio data into the new buffer
        alBufferData(m_BufferID,
                     AL_FORMAT_STEREO16,
                     pFileBuffer,
                     fileSize,
                     sampling);

        // grab a source ID from openAL
        alGenSources(1, @m_ID);

        // attach the buffer to the source
        alSourcei(m_ID, AL_BUFFER, m_BufferID);

        // set some basic source preferences
        alSourcef(m_ID, AL_PITCH, 1.0);
    except
        on e: Exception do
        begin
            Release;
            raise e;
        end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.Play: Boolean;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    if (m_ID = CQR_ErrorID) then
    begin
        Result := False;
        Exit;
    end;

    if (Assigned(m_fOnPlay)) then
        m_fOnPlay(Self);

    alSourcePlay(m_ID);
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.Pause: Boolean;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    if (m_ID = CQR_ErrorID) then
    begin
        Result := False;
        Exit;
    end;

    if (Assigned(m_fOnPause)) then
        m_fOnPause(Self);

    alSourcePause(m_ID);
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.Stop: Boolean;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    if (m_ID = CQR_ErrorID) then
    begin
        Result := False;
        Exit;
    end;

    if (Assigned(m_fOnStop)) then
        m_fOnStop(Self);

    alSourceStop(m_ID);
    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.IsPlaying: Boolean;
var
    state: TALenum;
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
    begin
        Result := False;
        Exit;
    end;

    if (m_ID = CQR_ErrorID) then
    begin
        Result := False;
        Exit;
    end;

    alGetSourcei(m_ID, AL_SOURCE_STATE, @state);

    Result := (state = AL_PLAYING);
end;
//--------------------------------------------------------------------------------------------------
function TQRPlayerAL.ChangeVolume(const value: Single): Boolean;
begin
    if (m_ID = CQR_ErrorID) then
    begin
        Result := False;
        Exit;
    end;

    if ((value >= 0.0) and (value <= 1.0)) then
    begin
        // allowed to use OpenAL?
        if (not m_Allowed) then
        begin
            Result := False;
            Exit;
        end;

        alSourcef(m_ID, AL_GAIN, value);

        if (Assigned(m_fOnChangeVolume)) then
            m_fOnChangeVolume(Self, value);

        Result := True;
        Exit;
    end;

    Result := False;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRPlayerAL.Loop(value: Boolean);
begin
    // allowed to use OpenAL?
    if (not m_Allowed) then
        Exit;

    if (m_ID = CQR_ErrorID) then
        Exit;

    alSourcei(m_ID, AL_LOOPING, AL_TRUE);
end;
//--------------------------------------------------------------------------------------------------

end.
