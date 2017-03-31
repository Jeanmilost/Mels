// *************************************************************************************************
// * ==> UTQRMDLModelGroup ------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load and link all files composing the MDL together.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRMDLModelGroup;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     System.Zip,
     UTQRCommon,
     UTQRHelpers,
     UTQRFiles,
     UTQRGraphics,
     UTQR3D,
     UTQRLight,
     UTQRCollision,
     UTQRModel,
     UTQRMDL,
     UTQRModelGroup,
     UTQRLogging,
     UTQRThreading,
     UTQRVCLHelpers,
     Vcl.Graphics,
     Winapi.Windows;

type
    {$REGION 'Documentation'}
    {**
     MDL animation configuration file
    }
    {$ENDREGION}
    TQRMDLAnimCfgFile = class(TQRFramedModelAnimCfgFile)
        private
            m_StartLine: NativeUInt;
            m_CurLine:   NativeUInt;

        protected
            {$REGION 'Documentation'}
            {**
             Parses a word found in script line
             @param(word Word to parse)
             @param(lineNb Current parsing line number)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function ParseWord(const word: UnicodeString; lineNb: NativeUInt): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

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
            procedure Clear; override;
    end;

    {$REGION 'Documentation'}
    {**
     MDL texture palette
    }
    {$ENDREGION}
    TQRMDLPalette = array of TQRColor;

    {$REGION 'Documentation'}
    {**
     Generic MDL job
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MDL job is designed to load all the files composing the MDL
                     model, and provides the data to be used by the group
    }
    {$ENDREGION}
    TQRMDLJob = class(TQRModelJob)
        private
            m_pModel:             TQRMDLModel;
            m_Textures:           TQRTextures;
            m_pColor:             TQRColor;
            m_pAnimations:        TQRMDLAnimCfgFile;
            m_pLight:             TQRDirectionalLight;
            m_pDefaultMesh:       PQRMesh;
            m_Palette:            TQRMDLPalette;
            m_MaxTexture:         NativeUInt;
            m_DefaultFrameIndex:  NativeUInt;
            m_RhToLh:             Boolean;
            m_TextureLoaded:      Boolean;
            m_IsCanceled:         Boolean;
            m_FramedModelOptions: TQRFramedModelOptions;
            m_fOnLoadTexture:     TQRLoadMeshTextureEvent;

            {$REGION 'Documentation'}
            {**
             Ppopulates the palette to use for uncompress the texture
            }
            {$ENDREGION}
            procedure PopulatePalette;

        protected
            {$REGION 'Documentation'}
            {**
             Gets the model
             @return(The model)
            }
            {$ENDREGION}
            function GetModel: TQRMDLModel; virtual;

            {$REGION 'Documentation'}
            {**
             Gets a texture
             @param(index The texture index to get)
             @return(The texture)
            }
            {$ENDREGION}
            function GetTexture(index: NativeInt): TQRTexture; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the texture count
             @return(The texture count)
            }
            {$ENDREGION}
            function GetTextureCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Uncompresses the texture
             @param(pParser Model parser)
             @param(index Texture index)
             @param(pTexture Texture to populate)
            }
            {$ENDREGION}
            procedure UncompressTexture(const pParser: TQRMDLParser;
                                                index: NativeUInt;
                                             pTexture: Vcl.Graphics.TBitmap); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the model color
             @return(The model color)
            }
            {$ENDREGION}
            function GetColor: TQRColor; virtual;

            {$REGION 'Documentation'}
            {**
             Gets animations
             @return(Animations)
            }
            {$ENDREGION}
            function GetAnimations: TQRMDLAnimCfgFile; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the default mesh
             @return(The default mesh)
            }
            {$ENDREGION}
            function GetDefaultMesh: PQRMesh; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the framed model options
             @return(The model options)
            }
            {$ENDREGION}
            function GetFramedModelOptions: TQRFramedModelOptions; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the framed model options
             @param(options Model options)
            }
            {$ENDREGION}
            procedure SetFramedModelOptions(options: TQRFramedModelOptions); virtual;

            {$REGION 'Documentation'}
            {**
             Called when model texture should be loaded
             @br bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnLoadTexture; virtual;

            {$REGION 'Documentation'}
            {**
             Called when default mesh (i.e. mesh to show during long operation) should be created
            }
            {$ENDREGION}
            procedure OnCreateDefaultMesh; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                         const pColor: TQRColor;
                         const pLight: TQRDirectionalLight;
                               rhToLh: Boolean;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                    defaultFrameIndex: NativeUInt;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Cancels the job
            }
            {$ENDREGION}
            procedure Cancel; override;

            {$REGION 'Documentation'}
            {**
             Checks if job was canceled
             @return(@true if job was canceled, otherwise @false)
            }
            {$ENDREGION}
            function IsCanceled: Boolean; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the model
            }
            {$ENDREGION}
            property Model: TQRMDLModel read GetModel;

            {$REGION 'Documentation'}
            {**
             Gets texture at index
            }
            {$ENDREGION}
            property Texture[index: NativeInt]: TQRTexture read GetTexture;

            {$REGION 'Documentation'}
            {**
             Gets the texture count
            }
            {$ENDREGION}
            property TextureCount: NativeInt read GetTextureCount;

            {$REGION 'Documentation'}
            {**
             Gets the model color
            }
            {$ENDREGION}
            property Color: TQRColor read GetColor;

            {$REGION 'Documentation'}
            {**
             Gets the animations
            }
            {$ENDREGION}
            property Animations: TQRMDLAnimCfgFile read GetAnimations;

            {$REGION 'Documentation'}
            {**
             Gets the default mesh
            }
            {$ENDREGION}
            property DefaultMesh: PQRMesh read GetDefaultMesh;

            {$REGION 'Documentation'}
            {**
             Gets or sets the framed model options
            }
            {$ENDREGION}
            property FramedModelOptions: TQRFramedModelOptions read GetFramedModelOptions write SetFramedModelOptions;
    end;

    {$REGION 'Documentation'}
    {**
     Job to load MDL from file
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MDL job is designed to load all the files composing the MDL
                     model, and provides the data to be used by the group
    }
    {$ENDREGION}
    TQRLoadMDLFileJob = class(TQRMDLJob)
        private
            m_Dir:  UnicodeString;
            m_Name: TFileName;

        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pGroup Group that owns the job)
             @param(dir Directory containing all the model files to load)
             @param(name Name that identifies the files belonging to the model in the directory,
                         e.g. 'Ogro' for Ogro.mdl, Ogro.cfg, ...)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                            const dir: UnicodeString;
                           const name: TFileName;
                         const pColor: TQRColor;
                         const pLight: TQRDirectionalLight;
                               rhToLh: Boolean;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                    defaultFrameIndex: NativeUInt;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Processes the job
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Process: Boolean; override;
    end;

    {$REGION 'Documentation'}
    {**
     Job to load MDL from memory dir
    }
    {$ENDREGION}
    TQRLoadMDLMemoryDirJob = class(TQRMDLJob)
        private
            m_pDir: TQRMemoryDir;
            m_Name: TFileName;

        protected
            {$REGION 'Documentation'}
            {**
             Called before a texture is loaded
             @param(pTexture Texture to load)
             @param(custom If @true, texture is a custom texture provided by the user, otherwise a
                    texture belonging to model)
            }
            {$ENDREGION}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {$REGION 'Documentation'}
            {**
             Called when a known texture should be loaded
             @param(pTexture Texture to load)
             @param(pBitmap Bitmap containing the loaded texture)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

            {$REGION 'Documentation'}
            {**
             Gets the memory dir containing model files
             @return(Memory dir)
            }
            {$ENDREGION}
            function GetMemoryDir: TQRMemoryDir; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(pDir Memory directory containing all model files to load)
             @param(name Name that identifies the files belonging to model in the directory, e.g.
                         'Ogro' for Ogro.mdl, Ogro.cfg, ...)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
             @br @bold(NOTE) Memory dir will be deleted internally, do not try to delete it from outside
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                           const pDir: TQRMemoryDir;
                           const name: TFileName;
                         const pColor: TQRColor;
                         const pLight: TQRDirectionalLight;
                               rhToLh: Boolean;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                    defaultFrameIndex: NativeUInt;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Processes the job
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Process: Boolean; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the memory directory
            }
            {$ENDREGION}
            property MemoryDir: TQRMemoryDir read GetMemoryDir;
    end;

    {$REGION 'Documentation'}
    {**
     Called when the model should be unpacked
     @param(pGroup Group at which model belongs)
     @param(pPackage MDL model package)
     @param(name @bold([out]) Model name without extension, as contained in package)
     @param(handled @bold([in, out]) If @true, model will be considered as unpacked and no further
                                     operation will be done. If @false, model will be unpacked using
                                     the standard algorithm)
     @return(@true on success, otherwise @false)
     @br @bold(NOTE) Be careful, newly added files in memory dir may conflict with unpacked files
                     while standard algorithm is applied if handled is set to @false
    }
    {$ENDREGION}
    TQRUnpackMDLModelEvent = function(const pGroup: TQRModelGroup;
                                          pPackage: TStream;
                                              pDir: TQRMemoryDir;
                                          out name: TFileName;
                                       var handled: Boolean): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Job to load MDL model from package (*.pk2 or .zip)
     @br @bold(NOTE) Some zip archives may be detected as valid but fails while stream is extracted,
                     by returning an incoherent stream content (no error is shown when this happen).
                     This seems to be a limitation of the zip library provided with the Embarcadero
                     Delphi compiler (XE7), and happen sometimes with some packages created with old
                     zippers, e.g. when RequiredVersion is set to 10 and CompressionMethod is set to
                     0 in the returned TZipHeader record. The solution for now is to extract and
                     recreate the package using a recent zipper
    }
    {$ENDREGION}
    TQRLoadMDLPackageJob = class(TQRLoadMDLMemoryDirJob)
        private
            m_pPackage:                TStream;
            m_ExternalUnpackSucceeded: Boolean;
            m_ExternalUnpackHandled:   Boolean;
            m_fOnUnpackModel:          TQRUnpackMDLModelEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Unpacks model package and prepare memory directory
             @return(@true on succes, otherwise @false)
            }
            {$ENDREGION}
            function Unpack: Boolean;

            {$REGION 'Documentation'}
            {**
             Called when model is about to be unpacked externally
            }
            {$ENDREGION}
            procedure OnUnpackModelExternally; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(pGroup Group that owns the job)
             @param(pPackage Stream containing package model to load)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
             @br @bold(NOTE) Package stream will be deleted internally, do not try to delete it from
                             outside
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                       const pPackage: TStream;
                         const pColor: TQRColor;
                         const pLight: TQRDirectionalLight;
                               rhToLh: Boolean;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                    defaultFrameIndex: NativeUInt;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Processes the job
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Process: Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the OnUnpackModel event
            }
            {$ENDREGION}
            property OnUnpackModel: TQRUnpackMDLModelEvent read m_fOnUnpackModel write m_fOnUnpackModel;
    end;

    {$REGION 'Documentation'}
    {**
     MDL model group, contains all items and functions needed to manage a complete MDL model
    }
    {$ENDREGION}
    TQRMDLGroup = class(TQRFramedModelGroup)
        private
            m_pJob:             TQRMDLJob;               // contains in-memory model after job ends
            m_pAnimation:       TQRFramedModelAnimation; // current running animation
            m_Gesture:          NativeInt;               // current running gesture
            m_PostponedGesture: NativeInt;               // gesture to set after model will be loaded
            m_RuningGesture:    NativeInt;               // currently running gesture (while cache is loaded)
            m_StartFrame:       NativeUInt;              // current gesture start frame index
            m_EndFrame:         NativeUInt;              // current gesture end frame index
            m_LoopFrame:        NativeUInt;              // current gesture frame loop index
            m_FPS:              NativeUInt;              // current gesture frame per seconds
            m_EndNotified:      Boolean;                 // if true, current animation reached end

        protected
            {$REGION 'Documentation'}
            {**
             Gets the animation gesture
             @return(The animation gesture)
            }
            {$ENDREGION}
            function GetGesture: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the animation gesture
             @param(gesture Animation gesture index to set)
            }
            {$ENDREGION}
            procedure SetGesture(gesture: NativeInt); virtual;

            {$REGION 'Documentation'}
            {**
             Sets gesture temporarly, runs only if gesture is completely cached
             @param(gesture Animation gesture index to set)
            }
            {$ENDREGION}
            procedure SetGestureIfAvailable(gesture: NativeInt); virtual;

            {$REGION 'Documentation'}
            {**
             Animates the model
             @param(elapsedTime Elapsed time since last calculation)
            }
            {$ENDREGION}
            procedure AnimateModel(const elapsedTime: Double); virtual;

            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh
             @param(index Mesh index to calculate and get)
             @param(mesh @bold([out]) Mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMesh(index: NativeUInt; out mesh: TQRMesh); virtual;

            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh, from cache if available, otherwise calculates and caches it
             @param(index Mesh index to calculate and get)
             @param(mesh @bold([out]) Mesh)
             @param(pTree @bold([out]) Aligned-axis bounding box tree matching with mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMeshUseCache(index: NativeUInt;
                                         out pMesh: PQRMesh;
                                         out pTree: TQRAABBTree); virtual;

            {$REGION 'Documentation'}
            {**
             Draws dynamic model
            }
            {$ENDREGION}
            procedure DrawDynamicModel; virtual;

            {$REGION 'Documentation'}
            {**
             Draws the cached model
            }
            {$ENDREGION}
            procedure DrawCachedModel; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the memory dir containing model files
             @return(memory dir, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetMemoryDir: TQRMemoryDir; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Clears group
            }
            {$ENDREGION}
            procedure Clear; override;

            {$REGION 'Documentation'}
            {**
             Checks if group is empty
             @return(@true if model is empty, otherwise @false)
            }
            {$ENDREGION}
            function IsEmpty: Boolean; override;

            {$REGION 'Documentation'}
            {**
             Loads group from dir
             @param(dir Directory containing all model files to load)
             @param(name Name that identifies the files belonging to model in the directory, e.g.
                         'Ogro' for Ogro.mdl, Ogro.cfg, ...)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) A MDL model is generally composed by the following files:
                             @br - a .mdl file that contains the model itself
                             @br In addition, this model system requires a configuration file for
                             the animations, that is composed as follow:
                             @longcode(
                                       [frame start] [frame count] [frame loop] [fps]
                                       0             39            39           15    // MODEL_WALK
                                       40            45            45           10    // MODEL_WAIT
                                       ...
                                       )
                             @br If no animation file is provided, only the first model frame will
                             be processed
            }
            {$ENDREGION}
            function Load(const dir: UnicodeString;
                         const name: TFileName;
                       const pColor: TQRColor;
                       const pLight: TQRDirectionalLight;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions;
                  defaultFrameIndex: NativeUInt = 0): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from memory dir
             @param(pDir Memory directory containing all model streams to load)
             @param(name Name that identifies the files belonging to model in the directory, e.g.
                         'Ogro' for Ogro.mdl, Ogro.cfg, ...)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Memory dir will be deleted internally, do not try to delete it from outside
             @br @bold(NOTE) A MDL model is generally composed by the following files:
                             @br - a .mdl file that contains the model itself
                             @br In addition, this model system requires a configuration file for
                             the animations, that is composed as follow:
                             @longcode(
                                       [frame start] [frame count] [frame loop] [fps]
                                       0             39            39           15    // MODEL_WALK
                                       40            45            45           10    // MODEL_WAIT
                                       ...
                                       )
                             @br If no animation file is provided, only the first model frame will
                             be processed
            }
            {$ENDREGION}
            function Load(const pDir: TQRMemoryDir;
                          const name: TFileName;
                        const pColor: TQRColor;
                        const pLight: TQRDirectionalLight;
                              rhToLh: Boolean;
                        modelOptions: TQRModelOptions;
                  framedModelOptions: TQRFramedModelOptions;
                   defaultFrameIndex: NativeUInt = 0): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from package (.pk2 or .zip) file
             @param(fileName Model package file name to load)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName;
                            const pColor: TQRColor;
                            const pLight: TQRDirectionalLight;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                       defaultFrameIndex: NativeUInt = 0): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from package (.pk2 or .zip) stream
             @param(pPackage Stream containing model package to load)
             @param(pColor Model color)
             @param(pLight Pre-calculated light, ignored if @nil)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(defaultFrameIndex Index of the default frame to show while model is loaded)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Package stream will be deleted internally, do not try to delete it from outside
            }
            {$ENDREGION}
            function Load(const pPackage: TStream;
                            const pColor: TQRColor;
                            const pLight: TQRDirectionalLight;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                       defaultFrameIndex: NativeUInt = 0): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Checks if the model is loaded
             @return(@true if the model is loaded, otherwise @false)
            }
            {$ENDREGION}
            function Loaded: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Queries the job status
             @return(Job status)
            }
            {$ENDREGION}
            function QueryJobStatus: TQRModelJobStatus; override;

            {$REGION 'Documentation'}
            {**
             Draws group
             @param(elapsedTime Elapsed time since last draw)
            }
            {$ENDREGION}
            procedure Draw(const elapsedTime: Double); override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets animation gesture
            }
            {$ENDREGION}
            property Gesture: NativeInt read GetGesture write SetGesture;

            {$REGION 'Documentation'}
            {**
             Gets the memory directory
            }
            {$ENDREGION}
            property MemoryDir: TQRMemoryDir read GetMemoryDir;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMDLAnimCfgFile
//--------------------------------------------------------------------------------------------------
constructor TQRMDLAnimCfgFile.Create;
begin
    inherited Create;

    m_StartLine := 0;
    m_CurLine   := 0;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMDLAnimCfgFile.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLAnimCfgFile.ParseWord(const word: UnicodeString; lineNb: NativeUInt): Boolean;
var
    c:       WideChar;
    gesture: NativeInt;
begin
    // nothing to parse?
    if (Length(word) = 0) then
        Exit(True);

    // by default, each line contains 4 numeric values, that describes the animation
    for c in word do
        if ((c <> '\0') and (not TQRStringHelper.IsNumeric(c, False))) then
            Exit(False);

    // first item to parse?
    if (GetItemCount = 0) then
    begin
        // store the start line, it will be used later to find the animation type
        m_StartLine := lineNb;
        m_CurLine   := lineNb;
    end;

    // first animation value?
    if (Column = 0) then
    begin
        // get animation gesture (each line pos in file matchs with the IEGesture enumerator)
        gesture := (m_CurLine - m_StartLine);

        // create and populate new item, and add it to list
        SetItemCount(GetItemCount + 1);
        Items[GetItemCount - 1].m_Gesture := gesture;

        Inc(m_CurLine);
    end;

    // search for animation item value to set
    case Column of
        0: Items[GetItemCount - 1].m_StartFrame      := StrToInt(word);
        1: Items[GetItemCount - 1].m_FrameCount      := StrToInt(word);
        2: Items[GetItemCount - 1].m_LoopingFrames   := StrToInt(word);
        3: Items[GetItemCount - 1].m_FramesPerSecond := StrToInt(word);
    else
        Exit(False);
    end;

    IncColumn;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLAnimCfgFile.Clear;
begin
    inherited Clear;

    // reset values
    m_StartLine := 0;
    m_CurLine   := 0;
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLJob
//--------------------------------------------------------------------------------------------------
constructor TQRMDLJob.Create(pGroup: TQRModelGroup;
                       const pColor: TQRColor;
                       const pLight: TQRDirectionalLight;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions;
                  defaultFrameIndex: NativeUInt;
                     fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup, modelOptions);

    // create local variables
    m_pModel        := TQRMDLModel.Create;
    m_pAnimations   := TQRMDLAnimCfgFile.Create;
    m_MaxTexture    := 100;
    m_TextureLoaded := False;
    m_IsCanceled    := False;
    New(m_pDefaultMesh);

    // copy values needed to load the model
    m_pColor             := TQRColor.Create(pColor);
    m_RhToLh             := rhToLh;
    m_pLight             := pLight;
    m_FramedModelOptions := framedModelOptions;
    m_DefaultFrameIndex  := defaultFrameIndex;
    m_fOnLoadTexture     := fOnLoadTexture;

    PopulatePalette;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMDLJob.Destroy;
var
    pColor:   TQRColor;
    pTexture: TQRTexture;
begin
    m_pLock.Lock;

    try
        // clear color palette
        for pColor in m_Palette do
            pColor.Free;

        SetLength(m_Palette, 0);

        // clear textures
        for pTexture in m_Textures do
            pTexture.Free;

        SetLength(m_Textures, 0);

        // clear memory
        m_pModel.Free;
        m_pColor.Free;
        m_pAnimations.Free;
        m_pLight.Free;

        // delete default mesh
        if (Assigned(m_pDefaultMesh)) then
            Dispose(m_pDefaultMesh);
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TQRMDLJob.PopulatePalette;
type
    {**
     Internal record representing a RGB color
    }
    IQRRGB = record
        m_R: Byte;
        m_G: Byte;
        m_B: Byte;
    end;

const
    {**
     Default MDL model texture table
    }
    colors: array [0..255] of IQRRGB =
    (
        (m_R:   0; m_G:   0; m_B:   0), (m_R:  15; m_G:  15; m_B:  15), (m_R:  31; m_G:  31; m_B:  31),
        (m_R:  47; m_G:  47; m_B:  47), (m_R:  63; m_G:  63; m_B:  63), (m_R:  75; m_G:  75; m_B:  75),
        (m_R:  91; m_G:  91; m_B:  91), (m_R: 107; m_G: 107; m_B: 107), (m_R: 123; m_G: 123; m_B: 123),
        (m_R: 139; m_G: 139; m_B: 139), (m_R: 155; m_G: 155; m_B: 155), (m_R: 171; m_G: 171; m_B: 171),
        (m_R: 187; m_G: 187; m_B: 187), (m_R: 203; m_G: 203; m_B: 203), (m_R: 219; m_G: 219; m_B: 219),
        (m_R: 235; m_G: 235; m_B: 235), (m_R:  15; m_G:  11; m_B:   7), (m_R:  23; m_G:  15; m_B:  11),
        (m_R:  31; m_G:  23; m_B:  11), (m_R:  39; m_G:  27; m_B:  15), (m_R:  47; m_G:  35; m_B:  19),
        (m_R:  55; m_G:  43; m_B:  23), (m_R:  63; m_G:  47; m_B:  23), (m_R:  75; m_G:  55; m_B:  27),
        (m_R:  83; m_G:  59; m_B:  27), (m_R:  91; m_G:  67; m_B:  31), (m_R:  99; m_G:  75; m_B:  31),
        (m_R: 107; m_G:  83; m_B:  31), (m_R: 115; m_G:  87; m_B:  31), (m_R: 123; m_G:  95; m_B:  35),
        (m_R: 131; m_G: 103; m_B:  35), (m_R: 143; m_G: 111; m_B:  35), (m_R:  11; m_G:  11; m_B:  15),
        (m_R:  19; m_G:  19; m_B:  27), (m_R:  27; m_G:  27; m_B:  39), (m_R:  39; m_G:  39; m_B:  51),
        (m_R:  47; m_G:  47; m_B:  63), (m_R:  55; m_G:  55; m_B:  75), (m_R:  63; m_G:  63; m_B:  87),
        (m_R:  71; m_G:  71; m_B: 103), (m_R:  79; m_G:  79; m_B: 115), (m_R:  91; m_G:  91; m_B: 127),
        (m_R:  99; m_G:  99; m_B: 139), (m_R: 107; m_G: 107; m_B: 151), (m_R: 115; m_G: 115; m_B: 163),
        (m_R: 123; m_G: 123; m_B: 175), (m_R: 131; m_G: 131; m_B: 187), (m_R: 139; m_G: 139; m_B: 203),
        (m_R:   0; m_G:   0; m_B:   0), (m_R:   7; m_G:   7; m_B:   0), (m_R:  11; m_G:  11; m_B:   0),
        (m_R:  19; m_G:  19; m_B:   0), (m_R:  27; m_G:  27; m_B:   0), (m_R:  35; m_G:  35; m_B:   0),
        (m_R:  43; m_G:  43; m_B:   7), (m_R:  47; m_G:  47; m_B:   7), (m_R:  55; m_G:  55; m_B:   7),
        (m_R:  63; m_G:  63; m_B:   7), (m_R:  71; m_G:  71; m_B:   7), (m_R:  75; m_G:  75; m_B:  11),
        (m_R:  83; m_G:  83; m_B:  11), (m_R:  91; m_G:  91; m_B:  11), (m_R:  99; m_G:  99; m_B:  11),
        (m_R: 107; m_G: 107; m_B:  15), (m_R:   7; m_G:   0; m_B:   0), (m_R:  15; m_G:   0; m_B:   0),
        (m_R:  23; m_G:   0; m_B:   0), (m_R:  31; m_G:   0; m_B:   0), (m_R:  39; m_G:   0; m_B:   0),
        (m_R:  47; m_G:   0; m_B:   0), (m_R:  55; m_G:   0; m_B:   0), (m_R:  63; m_G:   0; m_B:   0),
        (m_R:  71; m_G:   0; m_B:   0), (m_R:  79; m_G:   0; m_B:   0), (m_R:  87; m_G:   0; m_B:   0),
        (m_R:  95; m_G:   0; m_B:   0), (m_R: 103; m_G:   0; m_B:   0), (m_R: 111; m_G:   0; m_B:   0),
        (m_R: 119; m_G:   0; m_B:   0), (m_R: 127; m_G:   0; m_B:   0), (m_R:  19; m_G:  19; m_B:   0),
        (m_R:  27; m_G:  27; m_B:   0), (m_R:  35; m_G:  35; m_B:   0), (m_R:  47; m_G:  43; m_B:   0),
        (m_R:  55; m_G:  47; m_B:   0), (m_R:  67; m_G:  55; m_B:   0), (m_R:  75; m_G:  59; m_B:   7),
        (m_R:  87; m_G:  67; m_B:   7), (m_R:  95; m_G:  71; m_B:   7), (m_R: 107; m_G:  75; m_B:  11),
        (m_R: 119; m_G:  83; m_B:  15), (m_R: 131; m_G:  87; m_B:  19), (m_R: 139; m_G:  91; m_B:  19),
        (m_R: 151; m_G:  95; m_B:  27), (m_R: 163; m_G:  99; m_B:  31), (m_R: 175; m_G: 103; m_B:  35),
        (m_R:  35; m_G:  19; m_B:   7), (m_R:  47; m_G:  23; m_B:  11), (m_R:  59; m_G:  31; m_B:  15),
        (m_R:  75; m_G:  35; m_B:  19), (m_R:  87; m_G:  43; m_B:  23), (m_R:  99; m_G:  47; m_B:  31),
        (m_R: 115; m_G:  55; m_B:  35), (m_R: 127; m_G:  59; m_B:  43), (m_R: 143; m_G:  67; m_B:  51),
        (m_R: 159; m_G:  79; m_B:  51), (m_R: 175; m_G:  99; m_B:  47), (m_R: 191; m_G: 119; m_B:  47),
        (m_R: 207; m_G: 143; m_B:  43), (m_R: 223; m_G: 171; m_B:  39), (m_R: 239; m_G: 203; m_B:  31),
        (m_R: 255; m_G: 243; m_B:  27), (m_R:  11; m_G:   7; m_B:   0), (m_R:  27; m_G:  19; m_B:   0),
        (m_R:  43; m_G:  35; m_B:  15), (m_R:  55; m_G:  43; m_B:  19), (m_R:  71; m_G:  51; m_B:  27),
        (m_R:  83; m_G:  55; m_B:  35), (m_R:  99; m_G:  63; m_B:  43), (m_R: 111; m_G:  71; m_B:  51),
        (m_R: 127; m_G:  83; m_B:  63), (m_R: 139; m_G:  95; m_B:  71), (m_R: 155; m_G: 107; m_B:  83),
        (m_R: 167; m_G: 123; m_B:  95), (m_R: 183; m_G: 135; m_B: 107), (m_R: 195; m_G: 147; m_B: 123),
        (m_R: 211; m_G: 163; m_B: 139), (m_R: 227; m_G: 179; m_B: 151), (m_R: 171; m_G: 139; m_B: 163),
        (m_R: 159; m_G: 127; m_B: 151), (m_R: 147; m_G: 115; m_B: 135), (m_R: 139; m_G: 103; m_B: 123),
        (m_R: 127; m_G:  91; m_B: 111), (m_R: 119; m_G:  83; m_B:  99), (m_R: 107; m_G:  75; m_B:  87),
        (m_R:  95; m_G:  63; m_B:  75), (m_R:  87; m_G:  55; m_B:  67), (m_R:  75; m_G:  47; m_B:  55),
        (m_R:  67; m_G:  39; m_B:  47), (m_R:  55; m_G:  31; m_B:  35), (m_R:  43; m_G:  23; m_B:  27),
        (m_R:  35; m_G:  19; m_B:  19), (m_R:  23; m_G:  11; m_B:  11), (m_R:  15; m_G:   7; m_B:   7),
        (m_R: 187; m_G: 115; m_B: 159), (m_R: 175; m_G: 107; m_B: 143), (m_R: 163; m_G:  95; m_B: 131),
        (m_R: 151; m_G:  87; m_B: 119), (m_R: 139; m_G:  79; m_B: 107), (m_R: 127; m_G:  75; m_B:  95),
        (m_R: 115; m_G:  67; m_B:  83), (m_R: 107; m_G:  59; m_B:  75), (m_R:  95; m_G:  51; m_B:  63),
        (m_R:  83; m_G:  43; m_B:  55), (m_R:  71; m_G:  35; m_B:  43), (m_R:  59; m_G:  31; m_B:  35),
        (m_R:  47; m_G:  23; m_B:  27), (m_R:  35; m_G:  19; m_B:  19), (m_R:  23; m_G:  11; m_B:  11),
        (m_R:  15; m_G:   7; m_B:   7), (m_R: 219; m_G: 195; m_B: 187), (m_R: 203; m_G: 179; m_B: 167),
        (m_R: 191; m_G: 163; m_B: 155), (m_R: 175; m_G: 151; m_B: 139), (m_R: 163; m_G: 135; m_B: 123),
        (m_R: 151; m_G: 123; m_B: 111), (m_R: 135; m_G: 111; m_B:  95), (m_R: 123; m_G:  99; m_B:  83),
        (m_R: 107; m_G:  87; m_B:  71), (m_R:  95; m_G:  75; m_B:  59), (m_R:  83; m_G:  63; m_B:  51),
        (m_R:  67; m_G:  51; m_B:  39), (m_R:  55; m_G:  43; m_B:  31), (m_R:  39; m_G:  31; m_B:  23),
        (m_R:  27; m_G:  19; m_B:  15), (m_R:  15; m_G:  11; m_B:   7), (m_R: 111; m_G: 131; m_B: 123),
        (m_R: 103; m_G: 123; m_B: 111), (m_R:  95; m_G: 115; m_B: 103), (m_R:  87; m_G: 107; m_B:  95),
        (m_R:  79; m_G:  99; m_B:  87), (m_R:  71; m_G:  91; m_B:  79), (m_R:  63; m_G:  83; m_B:  71),
        (m_R:  55; m_G:  75; m_B:  63), (m_R:  47; m_G:  67; m_B:  55), (m_R:  43; m_G:  59; m_B:  47),
        (m_R:  35; m_G:  51; m_B:  39), (m_R:  31; m_G:  43; m_B:  31), (m_R:  23; m_G:  35; m_B:  23),
        (m_R:  15; m_G:  27; m_B:  19), (m_R:  11; m_G:  19; m_B:  11), (m_R:   7; m_G:  11; m_B:   7),
        (m_R: 255; m_G: 243; m_B:  27), (m_R: 239; m_G: 223; m_B:  23), (m_R: 219; m_G: 203; m_B:  19),
        (m_R: 203; m_G: 183; m_B:  15), (m_R: 187; m_G: 167; m_B:  15), (m_R: 171; m_G: 151; m_B:  11),
        (m_R: 155; m_G: 131; m_B:   7), (m_R: 139; m_G: 115; m_B:   7), (m_R: 123; m_G:  99; m_B:   7),
        (m_R: 107; m_G:  83; m_B:   0), (m_R:  91; m_G:  71; m_B:   0), (m_R:  75; m_G:  55; m_B:   0),
        (m_R:  59; m_G:  43; m_B:   0), (m_R:  43; m_G:  31; m_B:   0), (m_R:  27; m_G:  15; m_B:   0),
        (m_R:  11; m_G:   7; m_B:   0), (m_R:   0; m_G:   0; m_B: 255), (m_R:  11; m_G:  11; m_B: 239),
        (m_R:  19; m_G:  19; m_B: 223), (m_R:  27; m_G:  27; m_B: 207), (m_R:  35; m_G:  35; m_B: 191),
        (m_R:  43; m_G:  43; m_B: 175), (m_R:  47; m_G:  47; m_B: 159), (m_R:  47; m_G:  47; m_B: 143),
        (m_R:  47; m_G:  47; m_B: 127), (m_R:  47; m_G:  47; m_B: 111), (m_R:  47; m_G:  47; m_B:  95),
        (m_R:  43; m_G:  43; m_B:  79), (m_R:  35; m_G:  35; m_B:  63), (m_R:  27; m_G:  27; m_B:  47),
        (m_R:  19; m_G:  19; m_B:  31), (m_R:  11; m_G:  11; m_B:  15), (m_R:  43; m_G:   0; m_B:   0),
        (m_R:  59; m_G:   0; m_B:   0), (m_R:  75; m_G:   7; m_B:   0), (m_R:  95; m_G:   7; m_B:   0),
        (m_R: 111; m_G:  15; m_B:   0), (m_R: 127; m_G:  23; m_B:   7), (m_R: 147; m_G:  31; m_B:   7),
        (m_R: 163; m_G:  39; m_B:  11), (m_R: 183; m_G:  51; m_B:  15), (m_R: 195; m_G:  75; m_B:  27),
        (m_R: 207; m_G:  99; m_B:  43), (m_R: 219; m_G: 127; m_B:  59), (m_R: 227; m_G: 151; m_B:  79),
        (m_R: 231; m_G: 171; m_B:  95), (m_R: 239; m_G: 191; m_B: 119), (m_R: 247; m_G: 211; m_B: 139),
        (m_R: 167; m_G: 123; m_B:  59), (m_R: 183; m_G: 155; m_B:  55), (m_R: 199; m_G: 195; m_B:  55),
        (m_R: 231; m_G: 227; m_B:  87), (m_R: 127; m_G: 191; m_B: 255), (m_R: 171; m_G: 231; m_B: 255),
        (m_R: 215; m_G: 255; m_B: 255), (m_R: 103; m_G:   0; m_B:   0), (m_R: 139; m_G:   0; m_B:   0),
        (m_R: 179; m_G:   0; m_B:   0), (m_R: 215; m_G:   0; m_B:   0), (m_R: 255; m_G:   0; m_B:   0),
        (m_R: 255; m_G: 243; m_B: 147), (m_R: 255; m_G: 247; m_B: 199), (m_R: 255; m_G: 255; m_B: 255),
        (m_R: 159; m_G:  91; m_B:  83)
    );

var
    arraySize, i: Integer;

begin
    arraySize := Length(colors);
    SetLength(m_Palette, arraySize);

    // initialize texture palette
    for i := 0 to arraySize - 1 do
    begin
        m_Palette[i]   := TQRColor.Create;
        m_Palette[i].R := colors[i].m_R;
        m_Palette[i].G := colors[i].m_G;
        m_Palette[i].B := colors[i].m_B;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetModel: TQRMDLModel;
begin
    m_pLock.Lock;
    Result := m_pModel;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetTexture(index: NativeInt): TQRTexture;
begin
    m_pLock.Lock;

    if (index >= Length(m_Textures)) then
        Exit(nil);

    Result := m_Textures[index];

    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetTextureCount: NativeInt;
begin
    m_pLock.Lock;
    Result := Length(m_Textures);
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLJob.UncompressTexture(const pParser: TQRMDLParser;
                                              index: NativeUInt;
                                           pTexture: Vcl.Graphics.TBitmap);
var
    skinLength, offset:  NativeUInt;
    width, height, x, y: Integer;
    pLineRGB:            PQRRGBTripleArray;
begin
    // no texture to populate?
    if (not Assigned(pTexture)) then
        Exit;

    // no skin?
    if (pParser.SkinCount = 0) then
        Exit;

    // get the length of a skin, in bytes
    skinLength := pParser.Skins[0].m_TexLen;

    // is skin length empty?
    if (skinLength = 0) then
        Exit;

    // get skin width and height
    width  := pParser.Header.m_SkinWidth;
    height := pParser.Header.m_SkinHeight;

    // configure texture bitmap
    pTexture.PixelFormat := pf24bit;
    pTexture.Width       := width;
    pTexture.Height      := height;

    offset := skinLength * index;

    // iterate through bitmap lines
    for y := 0 to height - 1 do
    begin
        // get pixels line from bitmap
        pLineRGB := PQRRGBTripleArray(pTexture.ScanLine[y]);

        // iterate through line pixels
        for x := 0 to width - 1 do
        begin
            // get indexed pixel from palette
            pLineRGB[x].rgbtRed   := m_Palette[pParser.Skins[0].m_Data[offset]].GetRed;
            pLineRGB[x].rgbtGreen := m_Palette[pParser.Skins[0].m_Data[offset]].GetGreen;
            pLineRGB[x].rgbtBlue  := m_Palette[pParser.Skins[0].m_Data[offset]].GetBlue;

            // calculate next pixel offset
            Inc(offset);
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetColor: TQRColor;
begin
    m_pLock.Lock;
    Result := m_pColor;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetAnimations: TQRMDLAnimCfgFile;
begin
    m_pLock.Lock;
    Result := m_pAnimations;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetDefaultMesh: PQRMesh;
begin
    m_pLock.Lock;
    Result := m_pDefaultMesh;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.GetFramedModelOptions: TQRFramedModelOptions;
begin
    m_pLock.Lock;
    Result := m_FramedModelOptions;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLJob.SetFramedModelOptions(options: TQRFramedModelOptions);
begin
    m_pLock.Lock;
    m_FramedModelOptions := options;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLJob.OnLoadTexture;
var
    textureIndex, skinCount, i: NativeInt;
    max:                        NativeUInt;
    loadNext:                   Boolean;
    pTexture:                   Vcl.Graphics.TBitmap;
begin
    m_pLock.Lock;

    try
        m_TextureLoaded := False;

        if (GetStatus = EQR_JS_Canceled) then
            Exit;

        skinCount := m_pModel.Parser.SkinCount;
        loadNext  := True;
        max       := 0;

        // iterate through skins to load
        for i := 0 to skinCount - 1 do
        begin
            // by default don't load another texture after the current one will be loaded, because
            // in the MDL standards, only one texture file belongs to model. However this value may
            // be modified externally by any user that handle the OnLoadTexture event, to load any
            // additional textures as required
            loadNext := False;

            // too many textures were loaded?
            if (max > m_MaxTexture) then
                raise Exception.Create('Too many textures were created');

            // add a new model texture to the texture list
            textureIndex := Length(m_Textures);
            SetLength(m_Textures, textureIndex + 1);
            m_Textures[textureIndex] := TQRTexture.Create;

            // notify that a texture is about to be loaded
            BeforeLoadTexture(m_Textures[textureIndex], False);

            pTexture := Vcl.Graphics.TBitmap.Create;

            try
                // normally the texture index represents the handle of the texture on the GPU memory,
                // but exceptionnaly this index is used here to keep the skin index, thus the
                // LoadTexture() function may receive it
                m_Textures[textureIndex].Index := i;

                // load texture
                if (LoadTexture(m_Textures[textureIndex], pTexture)) then
                begin
                    // don't forget to release the previously used texture index
                    m_Textures[textureIndex].Index := 0;

                    // notify that a texture is loading
                    if (Assigned(m_fOnLoadTexture)) then
                        if (not m_fOnLoadTexture(GetGroup,
                                                 m_pModel,
                                                 pTexture,
                                                 m_Textures[textureIndex],
                                                 loadNext))
                        then
                            Exit;
                end
                else
                    // don't forget to release the previously used texture index
                    m_Textures[textureIndex].Index := 0;

            finally
                pTexture.Free;
            end;

            Inc(max);
        end;

        // load next texture, until no more texture should be loaded
        while (loadNext) do
        begin
            // by default don't load another texture after the current one will be loaded, because
            // in the MDL standards, only one texture file belongs to model. However this value may
            // be modified externally by any user that handle the OnLoadTexture event, to load any
            // additional textures as required
            loadNext := False;

            // too many textures were loaded?
            if (max > m_MaxTexture) then
                raise Exception.Create('Too many textures were created');

            // add a new model texture to the texture list
            textureIndex := Length(m_Textures);
            SetLength(m_Textures, textureIndex + 1);
            m_Textures[textureIndex] := TQRTexture.Create;

            // notify that a texture is about to be loaded
            BeforeLoadTexture(m_Textures[textureIndex], True);

            // notify that a texture is loading
            if (Assigned(m_fOnLoadTexture)) then
                if (not m_fOnLoadTexture(GetGroup,
                                         m_pModel,
                                         nil,
                                         m_Textures[textureIndex],
                                         loadNext))
                then
                    Exit;

            Inc(max);
        end;

        m_TextureLoaded := True;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLJob.OnCreateDefaultMesh;
begin
    m_pLock.Lock;

    try
        // model contains no mesh?
        if ((m_pModel.GetMeshCount = 0) or (m_DefaultFrameIndex >= m_pModel.GetMeshCount)) then
            Exit;

        if (not m_pModel.GetMesh(m_DefaultFrameIndex, m_pDefaultMesh^, nil)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to create default MDL mesh - class name - ' +
                                           ClassName);
            {$endif}
        end;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLJob.Cancel;
begin
    m_pLock.Lock;
    m_IsCanceled := True;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLJob.IsCanceled: Boolean;
begin
    m_pLock.Lock;
    Result := m_IsCanceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMDLFileJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMDLFileJob.Create(pGroup: TQRModelGroup;
                                  const dir: UnicodeString;
                                 const name: TFileName;
                               const pColor: TQRColor;
                               const pLight: TQRDirectionalLight;
                                     rhToLh: Boolean;
                               modelOptions: TQRModelOptions;
                         framedModelOptions: TQRFramedModelOptions;
                          defaultFrameIndex: NativeUInt;
                             fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup,
                     pColor,
                     pLight,
                     rhToLh,
                     modelOptions,
                     framedModelOptions,
                     defaultFrameIndex,
                     fOnLoadTexture);

    // copy values needed to load the model
    m_Dir  := dir;
    m_Name := name;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMDLFileJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMDLFileJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    // populate texture
    pTexture.Name := 'qr_mdl';
    pTexture.Dir  := m_Dir;

    // in MDL the texture is contained inside the model file itself, so texture has no file name
    pTexture.FileName := ''
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLFileJob.LoadTexture(pTexture: TQRTexture;
                                        pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
        Exit(False);

    // uncompress the model texture. Don't forget, the thread was already locked in the caller
    // function, so the model parser can be accessed directly without locking the thread again
    UncompressTexture(m_pModel.Parser, pTexture.Index, pBitmap);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLFileJob.Process: Boolean;
var
    modelName, animCfgName:  TFileName;
    frameCount, i:           NativeUInt;
    textureLoaded:           Boolean;
    vertexFormat:            TQRVertexFormat;
    pMesh:                   PQRMesh;
    pTree:                   TQRAABBTree;
    progressStep, totalStep: Single;
    doCreateCache:           Boolean;
begin
    // if job was still loaded, don't reload it
    if (IsLoaded) then
        Exit(True);

    try
        Progress := 0.0;

        // build model file name
        modelName := TQRFileHelper.AppendDelimiter(m_Dir) + m_Name + '.mdl';

        // file exists?
        if (not FileExists(modelName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MDL model file not exists - ' +
                                           modelName                      +
                                           ' - class name - '             +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // load MDL model
        if (not m_pModel.Load(modelName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to load MDL model - file name - ' +
                                           modelName                                 +
                                           ' - class name - '                        +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // get mesh count
        frameCount := m_pModel.GetMeshCount;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in ModelOptions));

        // do create cache?
        if (doCreateCache) then
            // calculate step count
            totalStep := frameCount + 5.0
        else
            // get step count
            totalStep := 5.0;

        // calculate the value of a progress step
        progressStep := (100.0 / totalStep);

        // model is loaded, add one step to progress
        Progress := Progress + progressStep;

        // populate model options
        m_pModel.Color  := m_pColor;
        m_pModel.RHToLH := m_RhToLh;

        // set pre-calculated light, if needed
        if (Assigned(m_pLight)) then
            // copy light properties
            m_pModel.PreCalculatedLight.Assign(m_pLight);

        // normals are loaded, add one step to progress
        Progress := Progress + progressStep;

        // notify main interface that texture should be loaded, wait until function returns
        TThread.Synchronize(nil, OnLoadTexture);

        m_pLock.Lock;

        try
            textureLoaded := m_TextureLoaded;
        finally
            m_pLock.Unlock;
        end;

        // texture is created, add one step to progress
        Progress := Progress + progressStep;

        // do include colors?
        if (EQR_MO_Without_Colors in ModelOptions) then
            vertexFormat := []
        else
            vertexFormat := [EQR_VF_Colors];

        // normals loaded?
        if (not(EQR_MO_Without_Normals in ModelOptions)) then
            Include(vertexFormat, EQR_VF_Normals);

        // texture loaded?
        if (textureLoaded and (not(EQR_MO_Without_Textures in ModelOptions))) then
            Include(vertexFormat, EQR_VF_TexCoords);

        // set vertex format
        m_pModel.VertexFormat := vertexFormat;

        // do create cache and do show default frame while job is processed?
        if (doCreateCache and (EQR_FO_Show_Default_Frame in m_FramedModelOptions)) then
            // load default mesh to show while cache is prepared, wait until function returns
            TThread.Synchronize(nil, OnCreateDefaultMesh);

        // model is configured and default mesh is created, add one step to progress
        Progress := Progress + progressStep;

        // build animations config file name
        animCfgName := TQRFileHelper.AppendDelimiter(m_Dir) + m_Name + '.cfg';

        // animations config file exists?
        if (FileExists(animCfgName)) then
            // load it
            if (not m_pAnimations.Load(animCfgName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to load MDL model animations - file name - ' +
                                               animCfgName                                          +
                                               ' - class name - '                                   +
                                               ClassName);
                {$endif}

                m_pAnimations.Clear;
            end;

        // do not create cache?
        if (not doCreateCache) then
        begin
            IsLoaded := True;
            Exit(True);
        end;

        // animations are loaded, add one step to progress
        Progress := Progress + progressStep;

        // something to cache?
        if (frameCount > 0) then
            // iterate through frames to cache
            for i := 0 to frameCount - 1 do
            begin
                // create mesh
                New(pMesh);

                // do ignore collisions?
                if (not(EQR_MO_No_Collision in ModelOptions)) then
                    // create AABB tree
                    pTree := TQRAABBTree.Create
                else
                    pTree := nil;

                // get current mesh
                if (not m_pModel.GetMesh(i, pMesh^, pTree, IsCanceled)) then
                begin
                    {$ifdef DEBUG}
                        TQRLogHelper.LogToCompiler('MDL model frame creation failed or was canceled - name - ' +
                                                   m_Name                                                      +
                                                   ' - index - '                                               +
                                                   IntToStr(Int64(i))                                          +
                                                   ' - class name - '                                          +
                                                   ClassName);
                    {$endif}

                    // failed or canceled?
                    Dispose(pMesh);
                    pTree.Free;
                    Exit(False);
                end;

                // add mesh to cache, note that from now cache will take care of the pointer
                try
                    SetMesh(i, pMesh);
                except
                    Dispose(pMesh);
                end;

                // do ignore collisions?
                if (not(EQR_MO_No_Collision in ModelOptions)) then
                    // add tree to cache, note that from now cache will take care of the pointer
                    try
                        SetTree(i, pTree);
                    except
                        pTree.Free;
                    end;

                // a new frame was cached, add one step to progress
                Progress := Progress + progressStep;
            end;

        Progress := 100.0;
        IsLoaded := True;
        Result   := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMDLMemoryDirJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMDLMemoryDirJob.Create(pGroup: TQRModelGroup;
                                      const pDir: TQRMemoryDir;
                                      const name: TFileName;
                                    const pColor: TQRColor;
                                    const pLight: TQRDirectionalLight;
                                          rhToLh: Boolean;
                                    modelOptions: TQRModelOptions;
                              framedModelOptions: TQRFramedModelOptions;
                               defaultFrameIndex: NativeUInt;
                                  fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup,
                     pColor,
                     pLight,
                     rhToLh,
                     modelOptions,
                     framedModelOptions,
                     defaultFrameIndex,
                     fOnLoadTexture);

    // copy values needed to load the model
    m_pDir := pDir;
    m_Name := name;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMDLMemoryDirJob.Destroy;
begin
    m_pLock.Lock;

    try
        // clear memory
        m_pDir.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMDLMemoryDirJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    // populate texture
    pTexture.Name := 'qr_mdl';
    pTexture.Dir  := '';

    // in MDL the texture is contained inside the model file itself, so texture has no file name
    pTexture.FileName := ''
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLMemoryDirJob.LoadTexture(pTexture: TQRTexture;
                                             pBitmap: Vcl.Graphics.TBitmap): Boolean;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
        Exit(False);

    // uncompress the model texture. Don't forget, the thread was already locked in the caller
    // function, so the model parser can be accessed directly without locking the thread again
    UncompressTexture(m_pModel.Parser, pTexture.Index, pBitmap);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLMemoryDirJob.GetMemoryDir: TQRMemoryDir;
begin
    m_pLock.Lock;
    Result := m_pDir;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLMemoryDirJob.Process: Boolean;
var
    modelName, animCfgName:       TFileName;
    pModelStream, pAnimCfgStream: TStream;
    frameCount, i:                NativeUInt;
    textureLoaded:                Boolean;
    vertexFormat:                 TQRVertexFormat;
    pMesh:                        PQRMesh;
    pTree:                        TQRAABBTree;
    progressStep, totalStep:      Single;
    doCreateCache:                Boolean;
begin
    // if job was still loaded, don't reload it
    if (IsLoaded) then
        Exit(True);

    try
        Progress := 0.0;

        // build model file name
        modelName := m_Name + '.mdl';

        // file exists?
        if (not m_pDir.FileExists(modelName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MDL model stream not exist - ' +
                                           modelName                       +
                                           ' - class name - '              +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // get stream containing MDL data
        pModelStream := m_pDir.GetFile(modelName);

        // found it?
        if (not Assigned(pModelStream)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to get MDL model stream - ' +
                                           modelName                           +
                                           ' - class name - '                  +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // load MDL model
        if (not m_pModel.Load(pModelStream, pModelStream.Size)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to load MDL model - stream name - ' +
                                           modelName                                   +
                                           ' - class name - '                          +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in ModelOptions));

        // get mesh count
        frameCount := m_pModel.GetMeshCount;

        // do create cache?
        if (doCreateCache) then
            // calculate step count
            totalStep := frameCount + 5.0
        else
            // get step count
            totalStep := 5.0;

        // calculate the value of a progress step
        progressStep := (100.0 / totalStep);

        // model is loaded, add one step to progress
        Progress := Progress + progressStep;

        // populate model options
        m_pModel.Color  := m_pColor;
        m_pModel.RHToLH := m_RhToLh;

        // set pre-calculated light, if needed
        if (Assigned(m_pLight)) then
            // copy light properties
            m_pModel.PreCalculatedLight.Assign(m_pLight);

        // normals are loaded, add one step to progress
        Progress := Progress + progressStep;

        // notify main interface that texture should be loaded, wait until function returns
        TThread.Synchronize(nil, OnLoadTexture);

        m_pLock.Lock;

        try
            textureLoaded := m_TextureLoaded;
        finally
            m_pLock.Unlock;
        end;

        // texture is created, add one step to progress
        Progress := Progress + progressStep;

        // do include colors?
        if (EQR_MO_Without_Colors in ModelOptions) then
            vertexFormat := []
        else
            vertexFormat := [EQR_VF_Colors];

        // do include normals?
        if (not (EQR_MO_Without_Normals in ModelOptions)) then
            Include(vertexFormat, EQR_VF_Normals);

        // texture loaded?
        if (textureLoaded and (not(EQR_MO_Without_Textures in ModelOptions))) then
            Include(vertexFormat, EQR_VF_TexCoords);

        // set vertex format
        m_pModel.VertexFormat := vertexFormat;

        // do create cache and do show default frame while job is processed?
        if (doCreateCache and (EQR_FO_Show_Default_Frame in m_FramedModelOptions)) then
            // load default mesh to show while cache is prepared, wait until function returns
            TThread.Synchronize(nil, OnCreateDefaultMesh);

        // model is configured and default mesh is created, add one step to progress
        Progress := Progress + progressStep;

        // build animations config file name
        animCfgName := m_Name + '.cfg';

        // animations config file exists?
        if (m_pDir.FileExists(animCfgName)) then
        begin
            // get stream containing normals
            pAnimCfgStream := m_pDir.GetFile(animCfgName);

            // found it?
            if (Assigned(pAnimCfgStream)) then
                // load it
                if (not m_pAnimations.Load(pAnimCfgStream, pAnimCfgStream.Size)) then
                begin
                    {$ifdef DEBUG}
                        TQRLogHelper.LogToCompiler('Failed to load MDL model animations - file name - ' +
                                                   animCfgName                                          +
                                                   ' - class name - '                                   +
                                                   ClassName);
                    {$endif}

                    m_pAnimations.Clear;
                end;
        end;

        // do not create cache?
        if (not doCreateCache) then
        begin
            IsLoaded := True;
            Exit(True);
        end;

        // animations are loaded, add one step to progress
        Progress := Progress + progressStep;

        // something to cache?
        if (frameCount > 0) then
            // iterate through frames to cache
            for i := 0 to frameCount - 1 do
            begin
                // create mesh
                New(pMesh);

                // do ignore collisions?
                if (not(EQR_MO_No_Collision in ModelOptions)) then
                    // create AABB tree
                    pTree := TQRAABBTree.Create
                else
                    pTree := nil;

                // get current mesh
                if (not m_pModel.GetMesh(i, pMesh^, pTree, IsCanceled)) then
                begin
                    {$ifdef DEBUG}
                        TQRLogHelper.LogToCompiler('MDL model frame creation failed or was canceled - name - ' +
                                                   m_Name                                                      +
                                                   ' - index - '                                               +
                                                   ' - class name - ' + ClassName                              +
                                                   IntToStr(Int64(i)));
                    {$endif}

                    // failed or canceled?
                    Dispose(pMesh);
                    pTree.Free;
                    Exit(False);
                end;

                // add mesh to cache, note that from now cache will take care of the pointer
                try
                    SetMesh(i, pMesh);
                except
                    Dispose(pMesh);
                end;

                // do ignore collisions?
                if (not(EQR_MO_No_Collision in ModelOptions)) then
                    // add tree to cache, note that from now cache will take care of the pointer
                    try
                        SetTree(i, pTree);
                    except
                        pTree.Free;
                    end;

                // a new frame was cached, add one step to progress
                Progress := Progress + progressStep;
            end;

        Progress := 100.0;
        IsLoaded := True;
        Result   := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMDLPackageJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMDLPackageJob.Create(pGroup: TQRModelGroup;
                                const pPackage: TStream;
                                  const pColor: TQRColor;
                                  const pLight: TQRDirectionalLight;
                                        rhToLh: Boolean;
                                  modelOptions: TQRModelOptions;
                            framedModelOptions: TQRFramedModelOptions;
                             defaultFrameIndex: NativeUInt;
                                fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup,
                     nil,
                     '',
                     pColor,
                     pLight,
                     rhToLh,
                     modelOptions,
                     framedModelOptions,
                     defaultFrameIndex,
                     fOnLoadTexture);

    m_ExternalUnpackSucceeded := False;
    m_ExternalUnpackHandled   := False;
    m_fOnUnpackModel          := nil;

    // create local variables
    m_pDir := TQRMemoryDir.Create(True);

    // copy values needed to load the model
    m_pPackage := pPackage;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMDLPackageJob.Destroy;
begin
    m_pLock.Lock;

    try
        // clear memory
        m_pPackage.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLPackageJob.Unpack: Boolean;
var
    pZipFile:     TZipFile;
    pLocalHeader: TZipHeader;
    fileName:     TFileName;
    zipFileName:  string;
    pZipStream:   TStream;
    pFileStream:  TMemoryStream;
begin
    // no stream to load to?
    if (not Assigned(m_pPackage)) then
        Exit(False);

    try
        // do unpack externally?
        if (Assigned(m_fOnUnpackModel)) then
        begin
            TThread.Synchronize(nil, OnUnpackModelExternally);

            // external unpack failed?
            if (not m_ExternalUnpackSucceeded) then
                Exit(False);

            // external unpack was handled?
            if (m_ExternalUnpackHandled) then
                Exit(True);
        end;

        // create zipper instance
        pZipFile := TZipFile.Create;

        try
            // open zip file for read
            pZipFile.Open(m_pPackage, zmRead);

            // iterate through zipped files
            for zipFileName in pZipFile.FileNames do
            begin
                // get next zipped file name (in lower case and without path)
                fileName := LowerCase(TQRFileHelper.ExtractFileName(zipFileName,
                                                                    CQR_Zip_Dir_Delimiter));

                // get model name, if still not exist
                if (Length(m_Name) = 0) then
                    m_Name := TQRFileHelper.ExtractFileNameNoExt(fileName);

                // found a dir? (in this case file name cannot be found)
                if (Length(fileName) = 0) then
                    continue;

                // file already exists in memory dir?
                if (m_pDir.FileExists(fileName)) then
                begin
                    {$ifdef DEBUG}
                        TQRLogHelper.LogToCompiler('MDL - unpack - found duplicate - file should be unique in package - ' +
                                                   fileName                                                               +
                                                   ' - class name - '                                                     +
                                                   ClassName);
                    {$endif}

                    Exit(False);
                end;

                pZipStream := nil;

                try
                    // extract file from zip
                    pZipFile.Read(zipFileName, pZipStream, pLocalHeader);

                    // succeeded?
                    if (not Assigned(pZipStream)) then
                    begin
                        {$ifdef DEBUG}
                            TQRLogHelper.LogToCompiler('MDL - unpack - failed to extract stream from zip - ' +
                                                       fileName                                              +
                                                       ' - class name - '                                    +
                                                       ClassName);
                        {$endif}

                        Exit(False);
                    end;

                    // rewind zip stream
                    pZipStream.Position := 0;

                    // copy zip stream content to memory stream
                    pFileStream := TMemoryStream.Create;
                    pFileStream.CopyFrom(pZipStream, pZipStream.Size);
                    pFileStream.Position := 0;

                    // add file to memory dir
                    m_pDir.AddFile(fileName, pFileStream, False);
                finally
                    pZipStream.Free;
                end;
            end;
        finally
            pZipFile.Free;
        end;
    finally
        m_pLock.Lock;

        try
            // clear memory
            m_pPackage.Free;
            m_pPackage := nil;
        finally
            m_pLock.Unlock;
        end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMDLPackageJob.OnUnpackModelExternally;
begin
    m_pLock.Lock;

    try
        if (GetStatus = EQR_JS_Canceled) then
            Exit;

        // no event defined?
        if (not Assigned(m_fOnUnpackModel)) then
        begin
            m_ExternalUnpackSucceeded := False;
            Exit;
        end;

        // call event
        m_ExternalUnpackHandled   := True;
        m_ExternalUnpackSucceeded := m_fOnUnpackModel(GetGroup,
                                                      m_pPackage,
                                                      m_pDir,
                                                      m_Name,
                                                      m_ExternalUnpackHandled);
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMDLPackageJob.Process: Boolean;
begin
    // if job was still loaded, don't reload it
    if (IsLoaded) then
        Exit(True);

    Progress := 0.0;

    // unpack model package
    if (not Unpack) then
        Exit(False);

    Result := inherited Process;
end;
//--------------------------------------------------------------------------------------------------
// TQRMDLGroup
//--------------------------------------------------------------------------------------------------
constructor TQRMDLGroup.Create;
begin
    inherited Create;

    m_pJob             :=  nil;
    m_pAnimation       :=  TQRFramedModelAnimation.Create;
    m_Gesture          := -1;
    m_PostponedGesture := -1;
    m_RuningGesture    := -1;
    m_StartFrame       :=  0;
    m_EndFrame         :=  0;
    m_LoopFrame        :=  0;
    m_FPS              :=  0;
    m_EndNotified      :=  False;
    SwapYZ             :=  True;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMDLGroup.Destroy;
begin
    // delete model and his associated job, don't forget to unregister it from worker
    if (Assigned(m_pJob)) then
    begin
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    // delete animation
    m_pAnimation.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.GetGesture: NativeInt;
begin
    if (m_PostponedGesture > 0) then
        Result := m_PostponedGesture
    else
        Result := m_Gesture;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.SetGesture(gesture: NativeInt);
var
    pItem:                                       PQRModelAnimCfgItem;
    startFrame, endFrame, loopFrame, frameCount: NativeUInt;
begin
    // do postpone gesture?
    if (not Assigned(m_pJob) or (m_pJob.GetStatus <> EQR_JS_Done)) then
    begin
        m_PostponedGesture := gesture;
        Exit;
    end;

    // now model is loaded, reset postponed and running gestures (will no more use it from now)
    m_PostponedGesture := -1;
    m_RuningGesture    := -1;

    // nothing to do?
    if (m_Gesture = gesture) then
        Exit;

    // reset gesture
    m_Gesture     := -1;
    m_StartFrame  :=  0;
    m_EndFrame    :=  0;
    m_LoopFrame   :=  0;
    m_FPS         :=  0;
    m_EndNotified :=  False;

    // clear running animation
    m_pAnimation.FrameIndex              := 0;
    m_pAnimation.InterpolationFrameIndex := 0;
    m_pAnimation.InterpolationFactor     := 0.0;
    m_pAnimation.Loop                    := False;

    // is new gesture out of bounds?
    if (gesture >= NativeInt(m_pJob.Animations.GetItemCount)) then
        Exit;

    // get gesture item
    pItem := m_pJob.Animations.GetItem(gesture);

    // found it?
    if (not Assigned(pItem)) then
        Exit;

    // get frame count, and calculate start and end frames
    frameCount := m_pJob.Model.GetMeshCount;
    startFrame := pItem.m_StartFrame;
    endFrame   := pItem.m_StartFrame + pItem.m_FrameCount;

    // check if animation values are valid
    if ((startFrame >= frameCount) or (endFrame >= frameCount)) then
        Exit;

    // check if loop frame is valid
    if (pItem.m_LoopingFrames = 0) then
        loopFrame := 0
    else
    begin
        // calculate loop frame
        loopFrame := pItem.m_StartFrame + pItem.m_LoopingFrames;

        // is loop frame out of bounds?
        if (loopFrame >= endFrame) then
            loopFrame := endFrame;
    end;

    // configure animation to execute
    m_Gesture    := gesture;
    m_StartFrame := startFrame;
    m_EndFrame   := endFrame;
    m_LoopFrame  := loopFrame;
    m_FPS        := pItem.m_FramesPerSecond;

    // configure running animation
    m_pAnimation.FrameIndex              := m_StartFrame;
    m_pAnimation.InterpolationFrameIndex := m_StartFrame;
    m_pAnimation.Loop                    := Boolean(m_LoopFrame);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.SetGestureIfAvailable(gesture: NativeInt);
var
    pItem:                                       PQRModelAnimCfgItem;
    startFrame, endFrame, loopFrame, frameCount: NativeUInt;
begin
    // is gesture already running?
    if ((m_RuningGesture >= 0) and (m_RuningGesture = gesture)) then
        Exit;

    // can only be done if cache is enabled and if gesture should run immediately when available
    if (not(EQR_MO_Create_Cache                     in m_pJob.ModelOptions) or
        not(EQR_FO_Start_Anim_When_Gesture_Is_Ready in m_pJob.FramedModelOptions))
    then
        Exit;

    // in case model was loaded, abort the operation to not perturbate the normal gesture processing
    if (m_pJob.GetStatus = EQR_JS_Done) then
        Exit;

    // reset gesture
    m_RuningGesture := -1;
    m_StartFrame    :=  0;
    m_EndFrame      :=  0;
    m_LoopFrame     :=  0;
    m_FPS           :=  0;
    m_EndNotified   :=  False;

    // clear running animation
    m_pAnimation.FrameIndex              := 0;
    m_pAnimation.InterpolationFrameIndex := 0;
    m_pAnimation.InterpolationFactor     := 0.0;
    m_pAnimation.Loop                    := False;

    // is new gesture out of bounds?
    if (gesture >= NativeInt(m_pJob.Animations.GetItemCount)) then
        Exit;

    // get gesture item
    pItem := m_pJob.Animations.GetItem(gesture);

    // found it?
    if (not Assigned(pItem)) then
        Exit;

    // get frame count, and calculate start and end frames
    frameCount := m_pJob.Model.GetMeshCount;
    startFrame := pItem.m_StartFrame;
    endFrame   := pItem.m_StartFrame + pItem.m_FrameCount;

    // check if animation values are valid
    if ((startFrame >= frameCount) or (endFrame >= frameCount)) then
        Exit;

    // is gesture completely loaded in cache?
    if (not Assigned(m_pJob.Mesh[startFrame]) or not Assigned(m_pJob.Mesh[endFrame])) then
        Exit;

    // check if loop frame is valid
    if (pItem.m_LoopingFrames = 0) then
        loopFrame := 0
    else
    begin
        // calculate loop frame
        loopFrame := pItem.m_StartFrame + pItem.m_LoopingFrames;

        // is loop frame out of bounds?
        if (loopFrame >= endFrame) then
            loopFrame := endFrame;
    end;

    // configure animation to execute
    m_RuningGesture := gesture;
    m_StartFrame    := startFrame;
    m_EndFrame      := endFrame;
    m_LoopFrame     := loopFrame;
    m_FPS           := pItem.m_FramesPerSecond;

    // configure running animation
    m_pAnimation.FrameIndex              := m_StartFrame;
    m_pAnimation.InterpolationFrameIndex := m_StartFrame;
    m_pAnimation.Loop                    := Boolean(m_LoopFrame);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.AnimateModel(const elapsedTime: Double);
var
    frameIndex:          NativeUInt;
    interpolationFactor: Double;
    pAnim:               TQRFramedModelAnimation;
    ignoreGestureCheck:  Boolean;
begin
    pAnim := nil;

    // check if gesture check should be ignored
    ignoreGestureCheck := (m_pJob.GetStatus <> EQR_JS_Done) and
                          (EQR_FO_Start_Anim_When_Gesture_Is_Ready in m_pJob.FramedModelOptions);

    // animation end reached or no animation to run?
    if (m_EndNotified or ((m_Gesture < 0) and (not ignoreGestureCheck))) then
        Exit;

    try
        frameIndex          := m_pAnimation.FrameIndex;
        interpolationFactor := m_pAnimation.InterpolationFactor;

        // animate model
        pAnim := GetAnimation(m_pJob.Model,
                              m_Gesture,
                              elapsedTime,
                              m_FPS,
                              m_StartFrame,
                              m_EndFrame,
                              Boolean(m_LoopFrame),
                              frameIndex,
                              interpolationFactor,
                              m_EndNotified);

        // update animation values
        m_pAnimation.FrameIndex              := pAnim.FrameIndex;
        m_pAnimation.InterpolationFrameIndex := pAnim.InterpolationFrameIndex;
        m_pAnimation.InterpolationFactor     := pAnim.InterpolationFactor;
    finally
        pAnim.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.GetDynamicMesh(index: NativeUInt; out mesh: TQRMesh);
var
    frameCount: NativeUInt;
begin
    // get frame count
    frameCount := m_pJob.Model.GetMeshCount;

    // model contains meshes?
    if (frameCount = 0) then
        Exit;

    // is mesh index out of bounds?
    if (index >= frameCount) then
        Exit;

    // get mesh
    if (not m_pJob.Model.GetMesh(index,
                                 mesh,
                                 TQRAABBTree(nil),
                                 TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('MDL model frame creation failed - index - ' +
                                       IntToStr(index)                              +
                                       ' - class name - '                           +
                                       ClassName);
        {$endif}
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.GetDynamicMeshUseCache(index: NativeUInt;
                                         out pMesh: PQRMesh;
                                         out pTree: TQRAABBTree);
var
    frameCount:    NativeUInt;
    useCollisions: Boolean;
begin
    pMesh := nil;
    pTree := nil;

    // get frame count
    frameCount := m_pJob.Model.GetMeshCount;

    // model contains meshes?
    if (frameCount = 0) then
        Exit;

    // is mesh index out of bounds?
    if (index >= frameCount) then
        Exit;

    useCollisions := not (EQR_MO_No_Collision in m_pJob.ModelOptions);

    // get mesh from cache
    pMesh := m_pJob.Mesh[m_pAnimation.FrameIndex];

    // do create collision buffers?
    if (useCollisions) then
        // get AABB tree from cache
        pTree := m_pJob.AABBTree[m_pAnimation.FrameIndex];

    // found in cache?
    if (Assigned(pMesh) and ((not useCollisions) or Assigned(pTree))) then
        Exit;

    // create new mesh
    New(pMesh);

    // do create collision buffers?
    if (useCollisions) then
        // create new AABB tree
        pTree := TQRAABBTree.Create;

    // get mesh and calculate AABB tree, if needed
    if (not m_pJob.Model.GetMesh(m_pAnimation.FrameIndex,
                                 pMesh^,
                                 pTree,
                                 TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('MDL model frame creation failed - index - ' +
                                       IntToStr(m_pAnimation.FrameIndex)            +
                                       ' - class name - '                           +
                                       ClassName);
        {$endif}

        // failed?
        Dispose(pMesh);
        pTree.Free;
        Exit;
    end;

    // add meshes to cache, note that from now cache will take care of the pointer
    try
        m_pJob.Mesh[index] := pMesh;
    except
        Dispose(pMesh);
    end;

    // do create collision buffers?
    if (useCollisions) then
        // add tree to cache, note that from now cache will take care of the pointer
        try
            m_pJob.AABBTree[index] := pTree;
        except
            pTree.Free;
        end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.DrawDynamicModel;
var
    pMesh, pNextMesh: PQRMesh;
    interpolatedMesh: TQRMesh;
    pTree, pNextTree: TQRAABBTree;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
        Exit;

    // can use dynamic cache?
    if (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions) then
    begin
        // do interpolate?
        if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
        begin
            // get mesh to draw
            m_pJob.Model.GetMesh(m_pAnimation.FrameIndex,
                                 m_pAnimation.InterpolationFrameIndex,
                                 m_pAnimation.InterpolationFactor,
                                 interpolatedMesh,
                                 TQRIsCanceledEvent(nil));

            // draw mesh
            OnDrawItem(Self,
                       m_pJob.Model,
                       m_pJob.m_Textures,
                       GetMatrix,
                       m_pAnimation.FrameIndex,
                       m_pAnimation.InterpolationFrameIndex,
                       m_pAnimation.InterpolationFactor,
                       @interpolatedMesh,
                       nil,
                       nil,
                       nil);
        end
        else
            try
                // create meshes to draw
                New(pMesh);
                New(pNextMesh);

                // get meshes
                GetDynamicMesh(m_pAnimation.FrameIndex,              pMesh^);
                GetDynamicMesh(m_pAnimation.InterpolationFrameIndex, pNextMesh^);

                // draw mesh
                OnDrawItem(Self,
                           m_pJob.Model,
                           m_pJob.m_Textures,
                           GetMatrix,
                           m_pAnimation.FrameIndex,
                           m_pAnimation.InterpolationFrameIndex,
                           m_pAnimation.InterpolationFactor,
                           pMesh,
                           pNextMesh,
                           nil,
                           nil);
            finally
                // clear memory
                Dispose(pMesh);
                Dispose(pNextMesh);
            end;

        Exit;
    end;

    // get meshes and AABB trees from cache, create them if still not exist
    GetDynamicMeshUseCache(m_pAnimation.FrameIndex,              pMesh,     pTree);
    GetDynamicMeshUseCache(m_pAnimation.InterpolationFrameIndex, pNextMesh, pNextTree);

    // do interpolate?
    if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
    begin
        // interpolate meshes
        TQRModelHelper.Interpolate(m_pAnimation.InterpolationFactor,
                                   pMesh^,
                                   pNextMesh^,
                                   interpolatedMesh);

        // draw mesh
        OnDrawItem(Self,
                   m_pJob.Model,
                   m_pJob.m_Textures,
                   GetMatrix,
                   m_pAnimation.FrameIndex,
                   m_pAnimation.InterpolationFrameIndex,
                   m_pAnimation.InterpolationFactor,
                   @interpolatedMesh,
                   nil,
                   pTree,
                   pNextTree);
    end
    else
        // draw mesh
        OnDrawItem(Self,
                   m_pJob.Model,
                   m_pJob.m_Textures,
                   GetMatrix,
                   m_pAnimation.FrameIndex,
                   m_pAnimation.InterpolationFrameIndex,
                   m_pAnimation.InterpolationFactor,
                   pMesh,
                   pNextMesh,
                   pTree,
                   pNextTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.DrawCachedModel;
var
    interpolatedMesh: TQRMesh;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
        Exit;

    // are indexes out of bounds?
    if ((m_pAnimation.FrameIndex              >= m_pJob.MeshCount) or
        (m_pAnimation.InterpolationFrameIndex >= m_pJob.MeshCount))
    then
        Exit;

    // collision buffers were created?
    if (EQR_MO_No_Collision in m_pJob.ModelOptions) then
    begin
        // do interpolate?
        if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
        begin
            // interpolate meshes
            TQRModelHelper.Interpolate(m_pAnimation.InterpolationFactor,
                                       m_pJob.Mesh[m_pAnimation.FrameIndex]^,
                                       m_pJob.Mesh[m_pAnimation.InterpolationFrameIndex]^,
                                       interpolatedMesh);

            // draw mesh
            OnDrawItem(Self,
                       m_pJob.Model,
                       m_pJob.m_Textures,
                       GetMatrix,
                       m_pAnimation.FrameIndex,
                       m_pAnimation.InterpolationFrameIndex,
                       m_pAnimation.InterpolationFactor,
                       @interpolatedMesh,
                       nil,
                       nil,
                       nil);
        end
        else
            // draw mesh
            OnDrawItem(Self,
                       m_pJob.Model,
                       m_pJob.m_Textures,
                       GetMatrix,
                       m_pAnimation.FrameIndex,
                       m_pAnimation.InterpolationFrameIndex,
                       m_pAnimation.InterpolationFactor,
                       m_pJob.Mesh[m_pAnimation.FrameIndex],
                       m_pJob.Mesh[m_pAnimation.InterpolationFrameIndex],
                       nil,
                       nil);
    end
    else
    // do interpolate?
    if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
    begin
        // interpolate meshes
        TQRModelHelper.Interpolate(m_pAnimation.InterpolationFactor,
                                   m_pJob.Mesh[m_pAnimation.FrameIndex]^,
                                   m_pJob.Mesh[m_pAnimation.InterpolationFrameIndex]^,
                                   interpolatedMesh);

        // draw mesh
        OnDrawItem(Self,
                   m_pJob.Model,
                   m_pJob.m_Textures,
                   GetMatrix,
                   m_pAnimation.FrameIndex,
                   m_pAnimation.InterpolationFrameIndex,
                   m_pAnimation.InterpolationFactor,
                   @interpolatedMesh,
                   nil,
                   m_pJob.AABBTree[m_pAnimation.FrameIndex],
                   m_pJob.AABBTree[m_pAnimation.InterpolationFrameIndex])
    end
    else
        // draw mesh
        OnDrawItem(Self,
                   m_pJob.Model,
                   m_pJob.m_Textures,
                   GetMatrix,
                   m_pAnimation.FrameIndex,
                   m_pAnimation.InterpolationFrameIndex,
                   m_pAnimation.InterpolationFactor,
                   m_pJob.Mesh[m_pAnimation.FrameIndex],
                   m_pJob.Mesh[m_pAnimation.InterpolationFrameIndex],
                   m_pJob.AABBTree[m_pAnimation.FrameIndex],
                   m_pJob.AABBTree[m_pAnimation.InterpolationFrameIndex]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.GetMemoryDir: TQRMemoryDir;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(nil);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(nil);

    // is job a memory dir job?
    if (m_pJob is TQRLoadMDLMemoryDirJob) then
        // get and return memory dir
        Exit(TQRLoadMDLMemoryDirJob(m_pJob).MemoryDir);

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.Clear;
begin
    // previous job was created?
    if (Assigned(m_pJob)) then
    begin
        // delete previous job
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    // reset values
    m_Gesture          := -1;
    m_PostponedGesture := -1;
    m_StartFrame       :=  0;
    m_EndFrame         :=  0;
    m_LoopFrame        :=  0;
    m_FPS              :=  0;
    m_EndNotified      :=  False;

    // clear running animation
    m_pAnimation.FrameIndex              := 0;
    m_pAnimation.InterpolationFrameIndex := 0;
    m_pAnimation.InterpolationFactor     := 0.0;
    m_pAnimation.Loop                    := False;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.IsEmpty: Boolean;
begin
    Result := (not Assigned(m_pJob));
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.Load(const dir: UnicodeString;
                         const name: TFileName;
                       const pColor: TQRColor;
                       const pLight: TQRDirectionalLight;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions;
                  defaultFrameIndex: NativeUInt): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRLoadMDLFileJob.Create(Self,
                                       dir,
                                       name,
                                       pColor,
                                       pLight,
                                       rhToLh,
                                       modelOptions,
                                       framedModelOptions,
                                       defaultFrameIndex,
                                       OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.Load(const pDir: TQRMemoryDir;
                          const name: TFileName;
                        const pColor: TQRColor;
                        const pLight: TQRDirectionalLight;
                              rhToLh: Boolean;
                        modelOptions: TQRModelOptions;
                  framedModelOptions: TQRFramedModelOptions;
                   defaultFrameIndex: NativeUInt): Boolean;
begin
    try
        // clear previous group instance
        Clear;
    except
        on e: Exception do
        begin
            // clear memory
            pDir.Free;

            // rethrow exception
            raise e;
        end;
    end;

    // prepare model job to load from file
    m_pJob := TQRLoadMDLMemoryDirJob.Create(Self,
                                            pDir,
                                            name,
                                            pColor,
                                            pLight,
                                            rhToLh,
                                            modelOptions,
                                            framedModelOptions,
                                            defaultFrameIndex,
                                            OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.Load(const fileName: TFileName;
                            const pColor: TQRColor;
                            const pLight: TQRDirectionalLight;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                       defaultFrameIndex: NativeUInt): Boolean;
var
    pPackage: TFileStream;
begin
    // file exists?
    if (not FileExists(fileName)) then
        Exit(False);

    // is file a valid zip package?
    if (not TZipFile.IsValid(fileName)) then
        Exit(False);

    // open a stream from file
    pPackage := TFileStream.Create(fileName, fmOpenRead);

    // load package
    Result := Load(pPackage,
                   pColor,
                   pLight,
                   rhToLh,
                   modelOptions,
                   framedModelOptions,
                   defaultFrameIndex);
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.Load(const pPackage: TStream;
                            const pColor: TQRColor;
                            const pLight: TQRDirectionalLight;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                       defaultFrameIndex: NativeUInt): Boolean;
begin
    // is package defined?
    if (not Assigned(pPackage)) then
        Exit(False);

    try
        // clear previous group instance
        Clear;
    except
        on e: Exception do
        begin
            // clear memory
            pPackage.Free;

            // rethrow exception
            raise e;
        end;
    end;

    // prepare model job to load from package stream
    m_pJob := TQRLoadMDLPackageJob.Create(Self,
                                          pPackage,
                                          pColor,
                                          pLight,
                                          rhToLh,
                                          modelOptions,
                                          framedModelOptions,
                                          defaultFrameIndex,
                                          OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.Loaded: Boolean;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(False);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(False);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMDLGroup.QueryJobStatus: TQRModelJobStatus;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        // set default values
        JobStatus.Status   := EQR_JS_NotStarted;
        JobStatus.Progress := 0;
    end
    else
    begin
        // get status from running job
        JobStatus.Status   := m_pJob.GetStatus;
        JobStatus.Progress := Floor(m_pJob.Progress);
    end;

    Result := JobStatus;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMDLGroup.Draw(const elapsedTime: Double);
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        // do run animation immediately when current gesture is loaded?
        if ((EQR_MO_Create_Cache                     in m_pJob.ModelOptions) and
            (EQR_FO_Start_Anim_When_Gesture_Is_Ready in m_pJob.FramedModelOptions))
        then
        begin
            // check if current gesture is available and set it if yes
            if (m_PostponedGesture >= 0) then
                SetGestureIfAvailable(m_PostponedGesture)
            else
            if (m_Gesture >= 0) then
                SetGestureIfAvailable(m_Gesture);

            // animate model
            AnimateModel(elapsedTime);

            DrawCachedModel;
            Exit;
        end;

        // do show default frame while job is processed?
        if ((EQR_FO_Show_Default_Frame in m_pJob.FramedModelOptions) and
             Assigned(m_pJob.DefaultMesh)                            and
             Assigned(OnDrawItem))
        then
            // draw default mesh (waiting for cache is fully created)
            OnDrawItem(Self,
                       m_pJob.Model,
                       m_pJob.m_Textures,
                       GetMatrix,
                       0,
                       0,
                       0.0,
                       m_pJob.DefaultMesh,
                       nil,
                       nil,
                       nil);

        Exit;
    end;

    // set postponed gesture to allow animation to run
    if ((m_PostponedGesture <> -1) and (m_Gesture = -1)) then
        SetGesture(m_PostponedGesture);

    // animate model
    AnimateModel(elapsedTime);

    // can draw model from a previously built cache, do generate frames dynamically, or let user
    // take care of frames creation?
    if (EQR_MO_Create_Cache in m_pJob.ModelOptions) then
        DrawCachedModel
    else
    if ((EQR_MO_Dynamic_Frames          in m_pJob.ModelOptions) or
        (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions))
    then
        DrawDynamicModel
    else
    if (Assigned(OnCustomDrawItem))
    then
        // let user take care of drawing model
        OnCustomDrawItem(Self,
                         m_pJob.Model,
                         m_pJob.m_Textures,
                         GetMatrix,
                         m_pAnimation.FrameIndex,
                         m_pAnimation.InterpolationFrameIndex,
                         m_pAnimation.InterpolationFactor);
end;
//--------------------------------------------------------------------------------------------------

end.
