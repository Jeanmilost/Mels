//******************************************************************************
//* ==> UTQRPlayerAL ----------------------------------------------------------*
//******************************************************************************
//* Description: Wav sound player component using OpenAL                       *
//* Developer:   Jean-Milost Reymond                                           *
//* License:     MIT License                                                   *
//* Copyright:   (c) 2015 - 2016, this file is part of the Mels library        *
//******************************************************************************
//* MIT License                                                                *
//*                                                                            *
//* Permission is hereby granted, free of charge, to any person obtaining a    *
//* copy of this software and associated documentation files (the "Software"), *
//* to deal in the Software without restriction, including without limitation  *
//* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
//* and/or sell copies of the Software, and to permit persons to whom the      *
//* Software is furnished to do so, subject to the following conditions:       *
//*                                                                            *
//* The above copyright notice and this permission notice shall be included in *
//* all copies or substantial portions of the Software.                        *
//*                                                                            *
//* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
//* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
//* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
//* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
//* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
//* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
//* DEALINGS IN THE SOFTWARE.                                                  *
//******************************************************************************

{
 @abstract(@name is a WAV sound player component using OpenAL.)
 @image(Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRPlayerAL;

interface

uses System.Classes,
     System.SysUtils,
     openal,
     UTQRPlayer;

type
  {$REGION 'Documentation'}
  {
   Called when music player is playing
   @param(PtSender event sender)
  }
  {$ENDREGION}
  TQROnPlayerPlayEvent = procedure (PtSender: TObject) of object;

  {$REGION 'Documentation'}
  {
   Called when music player is pausing
   @param(PtSender event sender)
  }
  {$ENDREGION}
  TQROnPlayerPauseEvent = procedure (PtSender: TObject) of object;

  {$REGION 'Documentation'}
  {
   Called when music player is stopping
   @param(PtSender event sender)
  }
  {$ENDREGION}
  TQROnPlayerStopEvent = procedure (PtSender: TObject) of object;

  {$REGION 'Documentation'}
  {
   Called when volume is changed on music player
   @param(PtSender event sender)
   @param(Value new volume value between 0.0f (lowest) and 1.0f (highest))
  }
  {$ENDREGION}
  TQROnPlayerChangeVolumeEvent = procedure (PtSender: TObject; Value: Single) of object;

  {$REGION 'Documentation'}
  {
   Simple player using OpenAL
   @note(Here is the OpenAL official website: http://www.openal.org/)
  }
  {$ENDREGION}
  TQRPlayerAL = class(TComponent, IQRPlayer)
  private
    {$REGION 'Documentation'}
    {
     OpenAL device handle
    }
    {$ENDREGION}
    FPtDevice: TALCdevice;

    {$REGION 'Documentation'}
    {
     OpenAL device context handle
    }
    {$ENDREGION}
    FPtContext: TALCcontext;

    {$REGION 'Documentation'}
    {
     Sound identifier attributed by OpenAL
    }
    {$ENDREGION}
    FID: TALuint;

    {$REGION 'Documentation'}
    {
     Sound buffer identifier attributed by OpenAL
    }
    {$ENDREGION}
    FBufferID: TALuint;

    {$REGION 'Documentation'}
    {
     Sampling rate in Hertz
    }
    {$ENDREGION}
    FSampling: NativeUInt;

    {$REGION 'Documentation'}
    {
     WAV file name
    }
    {$ENDREGION}
    FWavName: TFileName;

    {$REGION 'Documentation'}
    {
     In-memory opened WAV file
    }
    {$ENDREGION}
    FPtWav: TMemoryStream;

    {$REGION 'Documentation'}
    {
     If @true, OpenAL is coorectly installed and player is allowed to run
    }
    {$ENDREGION}
    FAllowed: Boolean;

    {$REGION 'Documentation'}
    {
     OnPlay event function handler, see @link(TQROnPlayerPlayEvent)
    }
    {$ENDREGION}
    FFnOnPlay: TQROnPlayerPlayEvent;

    {$REGION 'Documentation'}
    {
     OnPause event function handler, see @link(TQROnPlayerPauseEvent)
    }
    {$ENDREGION}
    FFnOnPause: TQROnPlayerPauseEvent;

    {$REGION 'Documentation'}
    {
     OnStop event function handler, see @link(TQROnPlayerStopEvent)
    }
    {$ENDREGION}
    FFnOnStop: TQROnPlayerStopEvent;

    {$REGION 'Documentation'}
    {
     OnChangeVolume event function handler, see @link(TQROnPlayerChangeVolumeEvent)
    }
    {$ENDREGION}
    FFnOnChangeVolume: TQROnPlayerChangeVolumeEvent;

    {$REGION 'Documentation'}
    {
     Release OpenAL resources
    }
    {$ENDREGION}
    procedure Release;

  protected const
    {$REGION 'Documentation'}
    {
     Error identifier
    }
    {$ENDREGION}
    CQR_ErrorID: NativeUInt = $FFFFFFFF;

  protected
    {$REGION 'Documentation'}
    {
     Sets sampling rate
     @param(Sampling sampling rate)
    }
    {$ENDREGION}
    procedure SetSampling(Sampling: NativeUInt); virtual;

    {$REGION 'Documentation'}
    {
     Sets wave file name to load
     @param(FileName wav file name)
    }
    {$ENDREGION}
    procedure SetWavName(FileName: TFileName); virtual;

    {$REGION 'Documentation'}
    {
     Declares properties that will deal with DFM files
     @param(PtFiler DFM file manager)
    }
    {$ENDREGION}
    procedure DefineProperties(PtFiler: TFiler); override;

    {$REGION 'Documentation'}
    {
     Reads WAV content from DFM file
     @param(PtStream stream containing DFM data)
    *}
    {$ENDREGION}
    procedure ReadWav(PtStream: TStream); virtual;

    {$REGION 'Documentation'}
    {
     Writes WAV content to DFM file
     @param(PtStream DFM stream in which package should be written)
    }
    {$ENDREGION}
    procedure WriteWav(PtStream: TStream); virtual;

    {$REGION 'Documentation'}
    {
     Called after control was fully loaded from DFM stream
    }
    {$ENDREGION}
    procedure Loaded; override;

    {$REGION 'Documentation'}
    {
     Loads the wav file from memory stream
    }
    {$ENDREGION}
    procedure LoadWav; virtual;

  public
    {$REGION 'Documentation'}
    {
     Constructor
     @param(PtOwner component owner)
    }
    {$ENDREGION}
    constructor Create(PtOwner: TComponent); override;

    {$REGION 'Documentation'}
    {
     Destructor
    }
    {$ENDREGION}
    destructor Destroy; override;

    {$REGION 'Documentation'}
    {
     Opens sound file
     @param(PtFileBuffer file buffer)
     @param(FileSize file size)
     @param(Sampling sampling rate, default value is 44'100 Hertz)
     @return(@true on success, otherwise @false)
     @note(buffer content must be wav format or uncompressed)
    }
    {$ENDREGION}
    function Open(const PtFileBuffer: PByte;
                            FileSize: NativeUInt;
                            Sampling: NativeUInt = 44100): Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Plays sound
     @return(@true on success, otherwise @false)
    }
    {$ENDREGION}
    function Play: Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Pauses sound
     @return(@true on success, otherwise @false)
    }
    {$ENDREGION}
    function Pause: Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Stops sound
     @return(@true on success, otherwise @false)
    }
    {$ENDREGION}
    function Stop: Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Checks if playback is already playing
     @return(@true if playback is already playing, otherwise @false)
    }
    {$ENDREGION}
    function IsPlaying: Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Changes volume
     @param(Value volume value between 0.0f (lowest) and 1.0f (highest))
     @return(@true on success, otherwise @false)
    }
    {$ENDREGION}
    function ChangeVolume(const Value: Single): Boolean; virtual;

    {$REGION 'Documentation'}
    {
     Loops the music
     @param(Value whether or not sound should loop)
    }
    {$ENDREGION}
    procedure Loop(Value: Boolean); virtual;

  // properties
  published
    {$REGION 'Documentation'}
    {
     Sampling rate, default value is 44'100 Hertz
    }
    {$ENDREGION}
    property Sampling: NativeUInt read FSampling write SetSampling default 44100;

    {$REGION 'Documentation'}
    {
     WAV file name
    }
    {$ENDREGION}
    property WavName: TFileName read FWavName write SetWavName;

    {$REGION 'Documentation'}
    {
     OnPlay event
    }
    {$ENDREGION}
    property OnPlay: TQROnPlayerPlayEvent read FFnOnPlay write FFnOnPlay;

    {$REGION 'Documentation'}
    {
     OnPause event
    }
    {$ENDREGION}
    property OnPause: TQROnPlayerPauseEvent read FFnOnPause write FFnOnPause;

    {$REGION 'Documentation'}
    {
     OnStop event
    }
    {$ENDREGION}
    property OnStop: TQROnPlayerStopEvent read FFnOnStop write FFnOnStop;

    {$REGION 'Documentation'}
    {
     OnChangeVolume event
    }
    {$ENDREGION}
    property OnChangeVolume: TQROnPlayerChangeVolumeEvent read FFnOnChangeVolume write FFnOnChangeVolume;
  end;

implementation
//------------------------------------------------------------------------------
// TQRPlayerAL
//------------------------------------------------------------------------------
constructor TQRPlayerAL.Create(PtOwner: TComponent);
begin
  inherited Create(PtOwner);

  FPtDevice         := nil;
  FPtContext        := nil;
  FID               := CQR_ErrorID;
  FBufferID         := CQR_ErrorID;
  FSampling         := 44100;
  FPtWav            := TMemoryStream.Create;
  FAllowed          := Assigned(@alcOpenDevice) or InitOpenAL;
  FFnOnPlay         := nil;
  FFnOnPause        := nil;
  FFnOnStop         := nil;
  FFnOnChangeVolume := nil;
end;
//------------------------------------------------------------------------------
destructor TQRPlayerAL.Destroy;
begin
  // stop music before closing it
  if (IsPlaying) then
    Stop;

  Release;

  // clear memory
  FPtWav.Free;

  inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.Release;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
    Exit;

  // delete sources
  if (FID <> CQR_ErrorID) then
  begin
    alDeleteSources(1, @FID);
    FID := CQR_ErrorID;
  end;

  // delete buffers
  if (FBufferID <> CQR_ErrorID) then
  begin
    alDeleteBuffers(1, @FBufferID);
    FBufferID := CQR_ErrorID;
  end;

  // destroy context
  if (Assigned(FPtContext)) then
  begin
    alcMakeContextCurrent(nil);
    alcDestroyContext(FPtContext);
    FPtContext := nil;
  end;

  // close device
  if (Assigned(FPtDevice)) then
  begin
    alcCloseDevice(FPtDevice);
    FPtDevice := nil;
  end;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.SetSampling(Sampling: NativeUInt);
begin
  // nothing to do?
  if (FSampling = Sampling) then
    Exit;

  // stop music in case it was playing
  if (IsPlaying) then
    Stop;

  Release;

  FSampling := Sampling;

  LoadWav;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.SetWavName(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  // no changes?
  if (FileName = FWavName) then
    Exit;

  // stop music in case it was playing
  if (IsPlaying) then
    Stop;

  Release;

  // if component is currently loading, just update the name, the wav will be loaded later
  if (csLoading in ComponentState) then
  begin
    FWavName := FileName;
    Exit;
  end;

  // previous wav was loaded?
  if (FPtWav.Size > 0) then
    // clear it
    FPtWav.Clear;

  // file name isn't empty, file exists and is a wav file?
  if ((Length(FileName) = 0)     or
      (not FileExists(FileName)) or
      (LowerCase(ExtractFileExt(FileName)) <> '.wav'))
  then
  begin
    // clear previously loaded model
    FWavName := '';
    Exit;
  end;

  // update name and open file stream
  FWavName   := FileName;
  FileStream := TFileStream.Create(FWavName, fmOpenRead);

  try
    // copy package file content to memory
    FPtWav.CopyFrom(FileStream, FileStream.Size);
  finally
    FileStream.Free;
  end;

  LoadWav;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.DefineProperties(PtFiler: TFiler);
  function DoWriteWav: Boolean;
  begin
    if Assigned(PtFiler.Ancestor) then
      Result := not (PtFiler.Ancestor is TQRPlayerAL)
    else
      Result := FPtWav.Size > 0;
  end;
begin
  inherited DefineProperties(PtFiler);

  // register the properties that will load and save a binary data in DFM files
  PtFiler.DefineBinaryProperty('Package', ReadWav, WriteWav, DoWriteWav);
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.ReadWav(PtStream: TStream);
begin
  // previous package was loaded?
  if (FPtWav.Size > 0) then
    // clear it
    FPtWav.Clear;

  // read model package from DFM stream
  FPtWav.CopyFrom(PtStream, PtStream.Size);
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.WriteWav(PtStream: TStream);
begin
  // reset stream position to start
  FPtWav.Position := 0;

  // write model package to DFM stream
  PtStream.CopyFrom(FPtWav, FPtWav.Size);
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.Loaded;
begin
  inherited Loaded;

  LoadWav;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.LoadWav;
begin
  // is component in desing time?
  if (csDesigning in ComponentState) then
    Exit;

  // no data to load?
  if (FPtWav.Size = 0) then
    Exit;

  FPtWav.Position := 0;

  // open wav file
  Open(PByte(FPtWav.Memory), FPtWav.Size, FSampling);
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.Open(const PtFileBuffer: PByte;
                                    FileSize: NativeUInt;
                                    Sampling: NativeUInt): Boolean;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
  begin
    Result := False;
    Exit;
  end;

  Release;

  // no sound file to load?
  if (not(Assigned(PtFileBuffer)) or (FileSize = 0)) then
  begin
    Result := False;
    Exit;
  end;

  try
    // select the "preferred device"
    FPtDevice := alcOpenDevice(nil);

    // found it?
    if (not Assigned(FPtDevice)) then
    begin
      Result := False;
      Exit;
    end;

    // use the device to make a context
    FPtContext := alcCreateContext(FPtDevice, nil);

    // found it?
    if (not Assigned(FPtContext)) then
    begin
      Release;
      Result := False;
      Exit;
    end;

    // set context to the currently active one
    alcMakeContextCurrent(FPtContext);

    // grab a buffer ID from openAL
    alGenBuffers(1, @FBufferID);

    // jam the audio data into the new buffer
    alBufferData(FBufferID,
                 AL_FORMAT_STEREO16,
                 PtFileBuffer,
                 FileSize,
                 Sampling);

    // grab a source ID from openAL
    alGenSources(1, @FID);

    // attach the buffer to the source
    alSourcei(FID, AL_BUFFER, FBufferID);

    // set some basic source preferences
    alSourcef(FID, AL_PITCH, 1.0);
  except
    on e: Exception do
    begin
      Release;
      raise e;
    end;
  end;

  Result := True;
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.Play: Boolean;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
  begin
    Result := False;
    Exit;
  end;

  if (FID = CQR_ErrorID) then
  begin
    Result := False;
    Exit;
  end;

  if (Assigned(FFnOnPlay)) then
    FFnOnPlay(Self);

  alSourcePlay(FID);
  Result := True;
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.Pause: Boolean;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
  begin
    Result := False;
    Exit;
  end;

  if (FID = CQR_ErrorID) then
  begin
    Result := False;
    Exit;
  end;

  if (Assigned(FFnOnPause)) then
    FFnOnPause(Self);

  alSourcePause(FID);
  Result := True;
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.Stop: Boolean;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
  begin
    Result := False;
    Exit;
  end;

  if (FID = CQR_ErrorID) then
  begin
    Result := False;
    Exit;
  end;

  if (Assigned(FFnOnStop)) then
    FFnOnStop(Self);

  alSourceStop(FID);
  Result := True;
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.IsPlaying: Boolean;
var
  State: TALenum;
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
  begin
    Result := False;
    Exit;
  end;

  if (FID = CQR_ErrorID) then
  begin
    Result := False;
    Exit;
  end;

  alGetSourcei(FID, AL_SOURCE_STATE, @state);

  Result := (state = AL_PLAYING);
end;
//------------------------------------------------------------------------------
function TQRPlayerAL.ChangeVolume(const Value: Single): Boolean;
begin
  if (FID = CQR_ErrorID) then
  begin
    Result := False;
    Exit;
  end;

  if ((Value >= 0.0) and (Value <= 1.0)) then
  begin
    // allowed to use OpenAL?
    if (not FAllowed) then
    begin
      Result := False;
      Exit;
    end;

    alSourcef(FID, AL_GAIN, value);

    if (Assigned(FFnOnChangeVolume)) then
      FFnOnChangeVolume(Self, value);

    Result := True;
    Exit;
  end;

    Result := False;
end;
//------------------------------------------------------------------------------
procedure TQRPlayerAL.Loop(value: Boolean);
begin
  // allowed to use OpenAL?
  if (not FAllowed) then
    Exit;

  if (FID = CQR_ErrorID) then
    Exit;

  alSourcei(FID, AL_LOOPING, AL_TRUE);
end;
//------------------------------------------------------------------------------

end.
