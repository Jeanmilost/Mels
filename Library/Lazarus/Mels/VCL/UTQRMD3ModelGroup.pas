// *************************************************************************************************
// * ==> UTQRMD3ModelGroup ------------------------------------------------------------------------*
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
 @abstract(@name provides the features to load and link all files composing the MD3 together.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRMD3ModelGroup;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     Math,
     Generics.Collections,
     Graphics,
     Zipper,
     UTQRCommon,
     UTQRHelpers,
     UTQRFiles,
     UTQRThreading,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRMD3,
     UTQRModelGroup;

type
    {$REGION 'Documentation'}
    {**
     MD3 model helper
    }
    {$ENDREGION}
    TQRMD3Helper = class
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Builds name based on a template
             @param(templateName Template name to build from)
             @param(prefixKeyword Prefix keyword to find and replace in template name)
             @param(prefix Prefix to use)
             @return(Built name)
            }
            {$ENDREGION}
            class function BuildName(const templateName,
                                          prefixKeyword,
                                                 prefix: UnicodeString): UnicodeString; static;

            {$REGION 'Documentation'}
            {**
             Builds tag name
             @param(itemName Item name)
             @return(Tag name, empty string if failed or on error)
            }
            {$ENDREGION}
            class function BuildTagName(const itemName: UnicodeString): UnicodeString; static;
    end;

    {$REGION 'Documentation'}
    {**
     Standard MD3 animation set, as commonly defined in the Quake engine
     @value(EQR_AG_MD3_Both_Death1 Selects the death nb. 1 gesture to be played by the model)
     @value(EQR_AG_MD3_Both_Dead1 Selects the dead nb. 1 gesture to be played by the model)
     @value(EQR_AG_MD3_Both_Death2 Selects the death nb. 2 gesture to be played by the model)
     @value(EQR_AG_MD3_Both_Dead2 Selects the dead nb. 2 gesture to be played by the model)
     @value(EQR_AG_MD3_Both_Death3 Selects the death nb. 3 gesture to be played by the model)
     @value(EQR_AG_MD3_Both_Dead3 Selects the dead nb. 3 gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Gesture Selects the torso default gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Attack Selects the torso attack gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Attack2 Selects the torso attack nb. 2 gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Drop Selects the torso drop gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Raise Selects the torso raise gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Stand Selects the torso stand gesture to be played by the model)
     @value(EQR_AG_MD3_Torso_Stand2 Selects the torso stand nb. 2 gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Walk_Crouching Selects the legs walk crouching gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Walk Selects the legs walk gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Run Selects the legs run gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Back Selects the legs back gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Swim Selects the legs swim gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Jump Selects the legs jump gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Land Selects the legs land gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Jump_Back Selects the legs jump back gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Land_Back Selects the legs land back gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Idle Selects the legs idle gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Idle_Crouching Selects the legs idle crouching gesture to be played by the model)
     @value(EQR_AG_MD3_Legs_Turn Selects the legs turn gesture to be played by the model)
     @value(EQR_AG_MD3_Max_Animations Indicates the animation gestures end, should not be used as a gesture)
     @br @bold(NOTE) These gestures are given for convenience, you are free to define your own
                     gestures. However these gestures must match with those defined in the model.cfg
                     file
    }
    {$ENDREGION}
    EQRMD3AnimationGesture =
    (
        EQR_AG_MD3_Both_Death1 = 0,
        EQR_AG_MD3_Both_Dead1,
        EQR_AG_MD3_Both_Death2,
        EQR_AG_MD3_Both_Dead2,
        EQR_AG_MD3_Both_Death3,
        EQR_AG_MD3_Both_Dead3,
        EQR_AG_MD3_Torso_Gesture,
        EQR_AG_MD3_Torso_Attack,
        EQR_AG_MD3_Torso_Attack2,
        EQR_AG_MD3_Torso_Drop,
        EQR_AG_MD3_Torso_Raise,
        EQR_AG_MD3_Torso_Stand,
        EQR_AG_MD3_Torso_Stand2,
        EQR_AG_MD3_Legs_Walk_Crouching,
        EQR_AG_MD3_Legs_Walk,
        EQR_AG_MD3_Legs_Run,
        EQR_AG_MD3_Legs_Back,
        EQR_AG_MD3_Legs_Swim,
        EQR_AG_MD3_Legs_Jump,
        EQR_AG_MD3_Legs_Land,
        EQR_AG_MD3_Legs_Jump_Back,
        EQR_AG_MD3_Legs_Land_Back,
        EQR_AG_MD3_Legs_Idle,
        EQR_AG_MD3_Legs_Idle_Crouching,
        EQR_AG_MD3_Legs_Turn,
        EQR_AG_MD3_Max_Animations
    );

    {$REGION 'Documentation'}
    {**
     MD3 model gender
     @value(EQR_GN_MD3_Unknown gender (i.e. undefined inside the model.cfg file))
     @value(EQR_GN_MD3_Male The character represented by the model is a male)
     @value(EQR_GN_MD3_Female The character represented by the model is a female)
    }
    {$ENDREGION}
    EQRMD3Gender =
    (
        EQR_GN_MD3_Unknown = 0,
        EQR_GN_MD3_Male,
        EQR_GN_MD3_Female
    );

    {$REGION 'Documentation'}
    {**
     Foot step mode
     @value(EQR_FS_MD3_Unknown Unknown foot step mode)
     @value(EQR_FS_MD3_Boot Boot foot step mode)
    }
    {$ENDREGION}
    EQRMD3FootStep =
    (
        EQR_FS_MD3_Unknown = 0,
        EQR_FS_MD3_Boot
    );

    {$REGION 'Documentation'}
    {**
     Structure containing headoffset instruction, don't know what exactly means
    }
    {$ENDREGION}
    TQRMD3AnimCfgFileHeadOffset = record
        {$REGION 'Documentation'}
        {**
         Unknown value nb.1, as found in the model.cfg file head offset section
        }
        {$ENDREGION}
        m_UnknownOffset1: NativeInt;

        {$REGION 'Documentation'}
        {**
         Unknown value nb.2, as found in the model.cfg file head offset section
        }
        {$ENDREGION}
        m_UnknownOffset2: NativeInt;

        {$REGION 'Documentation'}
        {**
         Unknown value nb.3, as found in the model.cfg file head offset section
        }
        {$ENDREGION}
        m_UnknownOffset3: NativeInt;
    end;

    PQRMD3AnimCfgFileHeadOffset = ^TQRMD3AnimCfgFileHeadOffset;

    {$REGION 'Documentation'}
    {**
     Structure containing footsteps instruction, don't know what exactly means
    }
    {$ENDREGION}
    TQRMD3AnimCfgFileFootSteps = record
        {$REGION 'Documentation'}
        {**
         Foot step mode
        }
        {$ENDREGION}
        m_Mode: EQRMD3FootStep;
    end;

    PQRMD3AnimCfgFileFootSteps = ^TQRMD3AnimCfgFileFootSteps;

    {$REGION 'Documentation'}
    {**
     MD3 animation configuration file parser
    }
    {$ENDREGION}
    TQRMD3AnimCfgFile = class(TQRFramedModelAnimCfgFile)
        private
            m_Gender:         EQRMD3Gender;
            m_HeadOffset:     TQRMD3AnimCfgFileHeadOffset;
            m_FootSteps:      TQRMD3AnimCfgFileFootSteps;
            m_StartLine:      NativeUInt;
            m_CurLine:        NativeUInt;
            m_ReadGender:     Boolean;
            m_ReadHeadOffset: Boolean;
            m_ReadFootSteps:  Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Called when script line should be parsed
             @param(line Line to parse)
             @param(linbeNb Line number)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean; override;

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
             Clears the animation configuration
            }
            {$ENDREGION}
            procedure Clear; override;

            {$REGION 'Documentation'}
            {**
             Gets the model gender
             @return(The model gender)
            }
            {$ENDREGION}
            function GetGender: EQRMD3Gender; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the head offset
             @return(The head offset)
            }
            {$ENDREGION}
            function GetHeadOffset: PQRMD3AnimCfgFileHeadOffset; virtual;

            {$REGION 'Documentation'}
            {**
             Gets foot steps
             @return(Foot steps)
            }
            {$ENDREGION}
            function GetFootSteps: PQRMD3AnimCfgFileFootSteps; virtual;

            {$REGION 'Documentation'}
            {**
             Converts the animation gesture to string
             @param(gesture Animation gesture to convert)
             @return(Converted animation gesture as string)
            }
            {$ENDREGION}
            class function GestureToStr(gesture: EQRMD3AnimationGesture): UnicodeString; static;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the model gender
            }
            {$ENDREGION}
            property Gender: EQRMD3Gender read GetGender;

            {$REGION 'Documentation'}
            {**
             Gets the head offset
            }
            {$ENDREGION}
            property HeadOffset: PQRMD3AnimCfgFileHeadOffset read GetHeadOffset;

            {$REGION 'Documentation'}
            {**
             Gets the foot steps
            }
            {$ENDREGION}
            property FootSteps: PQRMD3AnimCfgFileFootSteps read GetFootSteps;
    end;

    TQRMD3PathTable = TDictionary<UnicodeString, UnicodeString>;
    TQRMD3LinkKeys  = TStringList;

    {$REGION 'Documentation'}
    {**
     MD3 skin file parser
    }
    {$ENDREGION}
    TQRMD3Skin = class(TQRScript)
        private
            m_PathTable: TQRMD3PathTable;
            m_LinkKeys:  TQRMD3LinkKeys;

        protected
            {$REGION 'Documentation'}
            {**
             Called when a script line should be parsed
             @param(line Line to parse)
             @param(linbeNb Line number)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean; override;

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
             Clears the skin configuration
            }
            {$ENDREGION}
            procedure Clear; override;

            {$REGION 'Documentation'}
            {**
             Gets the path from the table matching with name
             @param(name Path name to get)
             @return(Path, empty string if not found or on error)
            }
            {$ENDREGION}
            function GetPath(const name: UnicodeString): UnicodeString; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the skin path matching with name
            }
            {$ENDREGION}
            property Path[const name: UnicodeString]: UnicodeString read GetPath;
    end;

    {$REGION 'Documentation'}
    {**
     Animation item, contains the information that defines a given animation
    }
    {$ENDREGION}
    TQRMD3AnimationItem = class
        private
            m_StartFrame:      NativeUInt;
            m_EndFrame:        NativeUInt;
            m_FramesPerSecond: NativeUInt;
            m_Loop:            Boolean;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(startFrame Animation start frame index)
             @param(endFrame Animation end frame index)
             @param(fps Frame per seconds)
             @param(loop If @true, animation should loop at end)
            }
            {$ENDREGION}
            constructor Create(startFrame, endFrame, fps: NativeUInt;
                                                    loop: Boolean); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the animation start frame index
            }
            {$ENDREGION}
            property StartFrame: NativeUInt read m_StartFrame write m_StartFrame;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation end frame index
            }
            {$ENDREGION}
            property EndFrame: NativeUInt read m_EndFrame write m_EndFrame;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation frame per seconds count
            }
            {$ENDREGION}
            property FramesPerSecond: NativeUInt read m_FramesPerSecond write m_FramesPerSecond;

            {$REGION 'Documentation'}
            {**
             Gets or sets if the animation should loop at end
            }
            {$ENDREGION}
            property Loop: Boolean read m_Loop write m_Loop;

    end;

    TQRMD3AnimationDictionary = TDictionary<EQRMD3AnimationGesture, TQRMD3AnimationItem>;

    {$REGION 'Documentation'}
    {**
     Link, it's a relationship between sub-models to animate
    }
    {$ENDREGION}
    TQRMD3ModelLink = class
        private
            m_TagIndex: NativeUInt;
            m_pItem:    Pointer;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Constructor
             @param(tagIndex Tag index in MD3 model from which link was built)
             @param(pItem Item to link to)
            }
            {$ENDREGION}
            constructor Create(tagIndex: NativeUInt; const pItem: Pointer); overload; virtual;

            {$REGION 'Documentation'}
            {**
              Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the MD3 model tag index
            }
            {$ENDREGION}
            property TagIndex: NativeUInt read m_TagIndex write m_TagIndex;

            {$REGION 'Documentation'}
            {**
             Gets or sets the linked item
            }
            {$ENDREGION}
            property Item: Pointer read m_pItem write m_pItem;
    end;

    TQRMD3ModelLinks = array of TQRMD3ModelLink;

    {$REGION 'Documentation'}
    {**
     Group sub-model item, contains all information about a particular sub-model item belonging to
     the group
    }
    {$ENDREGION}
    TQRMD3ModelItem = class
        private
            m_Name:        UnicodeString;
            m_pModel:      TQRMD3Model;
            m_pSkin:       TQRMD3Skin;
            m_Textures:    TQRTextures;
            m_LinksFrom:   TQRMD3ModelLinks;
            m_LinksTo:     TQRMD3ModelLinks;
            m_pAnimations: TQRMD3AnimationDictionary;
            m_pAnimation:  TQRFramedModelAnimation;
            m_Gesture:     EQRMD3AnimationGesture;
            m_CacheIndex:  NativeUInt;
            m_RhToLh:      Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Adds a target model in links
             @param(links Links in which target should be added)
             @param(pTarget Target to add to links)
             @param(tagIndex Model tag index from which link was generated)
            }
            {$ENDREGION}
            procedure AddLink(var links: TQRMD3ModelLinks;
                          const pTarget: TQRMD3ModelItem;
                               tagIndex: NativeUInt); virtual;

            {$REGION 'Documentation'}
            {**
             Gets model animation
             @param(gesture Animation gesture)
             @return(Animation item)
            }
            {$ENDREGION}
            function GetAnimation(gesture: EQRMD3AnimationGesture): TQRMD3AnimationItem; virtual;

            {$REGION 'Documentation'}
            {**
             Sets model animation
             @param(gesture Animationm gesture)
             @param(pAnimation Animation item to set)
            }
            {$ENDREGION}
            procedure SetAnimation(gesture: EQRMD3AnimationGesture;
                          const pAnimation: TQRMD3AnimationItem); virtual;

            {$REGION 'Documentation'}
            {**
             Gets model texture at index
             @param(index Texture index)
            }
            {$ENDREGION}
            function GetTexture(index: NativeInt): TQRTexture; virtual;

            {$REGION 'Documentation'}
            {**
             Gets link from a parent model
             @param(index Link index)
             @param(pTarget Linked parent model)
            }
            {$ENDREGION}
            function GetLinkFrom(index: NativeInt): TQRMD3ModelLink; virtual;

            {$REGION 'Documentation'}
            {**
             Gets link to a child model
             @param(index Link index)
             @param(pTarget Linked child model)
            }
            {$ENDREGION}
            function GetLinkTo(index: NativeInt): TQRMD3ModelLink; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the model texture count
             @return(The model texture count)
            }
            {$ENDREGION}
            function GetTextureCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the links from count
             @return(The links from count)
            }
            {$ENDREGION}
            function GetLinksFromCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the links to count
             @return(The links to count)
            }
            {$ENDREGION}
            function GetLinksToCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the model animation count
             @return(The model animation count)
            }
            {$ENDREGION}
            function GetAnimationCount: NativeInt; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Adds a model texture
             @param(pTexture Texture to add)
            }
            {$ENDREGION}
            procedure AddTexture(const pTexture: TQRTexture); virtual;

            {$REGION 'Documentation'}
            {**
             Adds a link from a parent model
             @param(tagIndex Model tag index from which link was generated)
             @param(pTarget Parent target model to link from)
            }
            {$ENDREGION}
            procedure AddLinkFrom(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem); virtual;

            {$REGION 'Documentation'}
            {**
             Adds link to a child model
             @param(tagIndex Model tag index from which link was generated)
             @param(pTarget Child target model to link to)
            }
            {$ENDREGION}
            procedure AddLinkTo(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem); virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the name
            }
            {$ENDREGION}
            property Name: UnicodeString read m_Name write m_Name;

            {$REGION 'Documentation'}
            {**
             Gets or sets the model
            }
            {$ENDREGION}
            property Model: TQRMD3Model read m_pModel write m_pModel;

            {$REGION 'Documentation'}
            {**
             Gets or sets the skin
            }
            {$ENDREGION}
            property Skin: TQRMD3Skin read m_pSkin write m_pSkin;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation from a gesture
            }
            {$ENDREGION}
            property Animations[gesture: EQRMD3AnimationGesture]: TQRMD3AnimationItem read GetAnimation write SetAnimation;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation
            }
            {$ENDREGION}
            property Animation: TQRFramedModelAnimation read m_pAnimation write m_pAnimation;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation gesture
            }
            {$ENDREGION}
            property Gesture: EQRMD3AnimationGesture read m_Gesture write m_Gesture;

            {$REGION 'Documentation'}
            {**
             Gets or sets if vertex buffer should be converted from left hand coordinates to right hand
            }
            {$ENDREGION}
            property RhToLh: Boolean read m_RhToLh write m_RhToLh;

            {$REGION 'Documentation'}
            {**
             Gets texture at index
            }
            {$ENDREGION}
            property Textures[index: NativeInt]: TQRTexture read GetTexture;

            {$REGION 'Documentation'}
            {**
             Gets the link from at index
            }
            {$ENDREGION}
            property LinksFrom[index: NativeInt]: TQRMD3ModelLink read GetLinkFrom;

            {$REGION 'Documentation'}
            {**
             Gets the link to at index
            }
            {$ENDREGION}
            property LinksTo[index: NativeInt]: TQRMD3ModelLink read GetLinkTo;

            {$REGION 'Documentation'}
            {**
             Gets the texture count
            }
            {$ENDREGION}
            property TextureCount: NativeInt read GetTextureCount;

            {$REGION 'Documentation'}
            {**
             Gets the link from count
            }
            {$ENDREGION}
            property LinksFromCount: NativeInt read GetLinksFromCount;

            {$REGION 'Documentation'}
            {**
             Gets the link to count
            }
            {$ENDREGION}
            property LinksToCount: NativeInt read GetLinksToCount;

            {$REGION 'Documentation'}
            {**
             Gets the animation count
            }
            {$ENDREGION}
            property AnimationCount: NativeInt read GetAnimationCount;

            {$REGION 'Documentation'}
            {**
             Gets the cache index
            }
            {$ENDREGION}
            property CacheIndex: NativeUInt read m_CacheIndex;
    end;

    TQRMD3ModelItems = array of TQRMD3ModelItem;

    {$REGION 'Documentation'}
    {**
     Group info, contains all informations to load group, as e.g. file templates, prefixes, ...
    }
    {$ENDREGION}
    TQRMD3GroupInfo = class
        private
            m_ModelTemplate: UnicodeString;
            m_SkinTemplate:  UnicodeString;
            m_AnimTemplate:  UnicodeString;
            m_pPrefixes:     TStringList;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
             @param(createDefaultPrefixes If @true, default prefixes will be created)
             @br @bold(NOTE) First declared prefix always matchs with the root model
             @br @bold(NOTE) Be careful, the prefix order will influence on the manner the objects
                             will be linked. For example, a head/upper/lower order will link the
                             head to upper model, then the upper to lower model, whereas a
                             lower/upper/head order will link the lower to upper model, then the
                             upper to head model
            }
            {$ENDREGION}
            constructor Create(createDefaultPrefixes: Boolean = true); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Copy constructor
             @param(pOther Other group info to copy from)
            }
            {$ENDREGION}
            constructor Create(const pOther: TQRMD3GroupInfo); overload; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets prefix at index
             @param(index Prefix index to get)
             @return(Prefix)
            }
            {$ENDREGION}
            function GetPrefix(index: NativeInt): UnicodeString; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the prefix count
             @return(The prefix count)
            }
            {$ENDREGION}
            function GetPrefixCount: NativeInt; virtual;

            {$REGION 'Documentation'}
            {**
             Adds prefix to list
             @param(prefix Prefix to add)
            }
            {$ENDREGION}
            procedure AddPrefix(const prefix: UnicodeString); virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets or sets the model template file name to use
            }
            {$ENDREGION}
            property ModelTemplate: UnicodeString read m_ModelTemplate write m_ModelTemplate;

            {$REGION 'Documentation'}
            {**
             Gets or sets the skin template file name to use
            }
            {$ENDREGION}
            property SkinTemplate: UnicodeString read m_SkinTemplate  write m_SkinTemplate;

            {$REGION 'Documentation'}
            {**
             Gets or sets the animation template file name to use
            }
            {$ENDREGION}
            property AnimTemplate: UnicodeString read m_AnimTemplate  write m_AnimTemplate;

            {$REGION 'Documentation'}
            {**
             Gets the prefix at index
            }
            {$ENDREGION}
            property Prefixes[index: NativeInt]: UnicodeString read GetPrefix;

            {$REGION 'Documentation'}
            {**
             Gets the prefix count
            }
            {$ENDREGION}
            property PrefixCount: NativeInt read GetPrefixCount;
    end;

    TQRMD3ItemDictionary = TDictionary<UnicodeString, TQRMD3ModelItem>;

    {$REGION 'Documentation'}
    {**
     Generic MD3 job
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MD3 job is designed to load all the files composing the MD3
                     model, and provides the data to be used by the group
    }
    {$ENDREGION}
    TQRMD3Job = class(TQRModelJob)
        private
            m_Items:              TQRMD3ModelItems;
            m_pItemDictionary:    TQRMD3ItemDictionary;
            m_pInfo:              TQRMD3GroupInfo;
            m_pColor:             TQRColor;
            m_TextureName:        UnicodeString;
            m_TextureFileName:    UnicodeString;
            m_ItemIndex:          NativeInt;
            m_MaxTexture:         NativeUInt;
            m_TextureLoaded:      Boolean;
            m_IsCanceled:         Boolean;
            m_FramedModelOptions: TQRFramedModelOptions;
            m_fOnLoadTexture:     TQRLoadMeshTextureEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Gets model item at index
             @return(Model item)
            }
            {$ENDREGION}
            function GetItem(index: NativeInt): TQRMD3ModelItem; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Gets model item from name
             @return(Model item)
            }
            {$ENDREGION}
            function GetItem(const name: UnicodeString): TQRMD3ModelItem; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Gets framed model options
             @return(Model options)
            }
            {$ENDREGION}
            function GetFramedModelOptions: TQRFramedModelOptions; virtual;

            {$REGION 'Documentation'}
            {**
             Sets framed model options
             @param(options Model options)
            }
            {$ENDREGION}
            procedure SetFramedModelOptions(options: TQRFramedModelOptions); virtual;

            {$REGION 'Documentation'}
            {**
             Links all models loaded or added in group together
            }
            {$ENDREGION}
            procedure LinkModel; virtual;

            {$REGION 'Documentation'}
            {**
             Links skin elements with model
             @param(pSkin Skin containing elements to load)
             @param(dir Model directory)
             @param(pItem Item containing the sub-model to link with skin)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LinkSkin(const pSkin: TQRMD3Skin;
                                const dir: UnicodeString;
                              const pItem: TQRMD3ModelItem): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Links animations with model
             @param(pInfo Group info)
             @param(pAnimations Configuration file containing animations)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LinkAnimations(const pInfo: TQRMD3GroupInfo;
                              const pAnimations: TQRMD3AnimCfgFile): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Called when model texture should be loaded
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnLoadTexture; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(pInfo Model info)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
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
             Gets model item at index
            }
            {$ENDREGION}
            property ItemAtIndex[index: NativeInt]: TQRMD3ModelItem read GetItem;

            {$REGION 'Documentation'}
            {**
             Gets model item from name
            }
            {$ENDREGION}
            property ItemFromName[const name: UnicodeString]: TQRMD3ModelItem read GetItem;

            {$REGION 'Documentation'}
            {**
             Gets or sets the framed model options
            }
            {$ENDREGION}
            property FramedModelOptions: TQRFramedModelOptions read GetFramedModelOptions write SetFramedModelOptions;
    end;

    {$REGION 'Documentation'}
    {**
     Job to load MD3 from file
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MD3 job is designed to load all the files composing the MD3
                     model, and provides the data to be used by the group
    }
    {$ENDREGION}
    TQRLoadMD3FileJob = class(TQRMD3Job)
        private
            m_Dir:           UnicodeString;
            m_PrefixKeyword: UnicodeString;

        protected
            {$REGION 'Documentation'}
            {**
             Loads sub-model skin file
             @param(fileName Skin file name)
             @param(pItem Item representing sub-model)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadSkin(const fileName: TFileName;
                                 const pItem: TQRMD3ModelItem): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads animations from animation configuration file
             @param(pInfo Model group info)
             @param(fileName Animation config file name)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                 const fileName: TFileName): Boolean; overload; virtual;

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
                                  pBitmap: Graphics.TBitmap): Boolean; override;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(dir Directory in which all model files to load are contained)
             @param(prefixKeyword Model templates prefix keyword)
             @param(pInfo Model group info)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                            const dir,
                        prefixKeyword: UnicodeString;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
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
     Job to load MD3 from memory directory
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MD3 job is designed to load all the files composing the MD3
                     model, and provides the data to be used by the group
    }
    {$ENDREGION}
    TQRLoadMD3MemoryDirJob = class(TQRMD3Job)
        private
            m_pDir:          TQRMemoryDir;
            m_PrefixKeyword: UnicodeString;

        protected
            {$REGION 'Documentation'}
            {**
             Loads sub-model skin file
             @param(pStream Stream containing the skin data)
             @param(pItem Item representing the sub-model)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadSkin(pStream: TStream; const pItem: TQRMD3ModelItem): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads animations from animation configuration file
             @param(pInfo Model group info)
             @param(pStream Duffer containing animation configuration)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                        pStream: TStream): Boolean; overload; virtual;

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
                                  pBitmap: Graphics.TBitmap): Boolean; override;

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
             @param(prefixKeyword Model templates prefix keyword)
             @param(pInfo Model group info)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @raises(Exception if group is not defined)
             @br @bold(NOTE) Memory dir will be deleted internally, do not try to delete it from outside
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                           const pDir: TQRMemoryDir;
                  const prefixKeyword: UnicodeString;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
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
     Teams at which the model belongs. Basically a .pk3 package contains 3 models versions (here
     called teams), a default, a green and a blue. Additionally some packages may contain some
     special features, in this case the team name should be specified
     @value(EQR_PT_MD3_Default No team defined for the model)
     @value(EQR_PT_MD3_Red Model belongs to the red team)
     @value(EQR_PT_MD3_Blue Model belongs to the blue team)
     @value(EQR_PT_MD3_Custom Model belongs to a custom user team)
    }
    {$ENDREGION}
    EQRMD3PackageTeam =
    (
        EQR_PT_MD3_Default = 0,
        EQR_PT_MD3_Red,
        EQR_PT_MD3_Blue,
        EQR_PT_MD3_Custom
    );

    {$REGION 'Documentation'}
    {**
     Sound files that the package may contain
     @value(EQR_PS_MD3_Death1 The death nb. 1 sound)
     @value(EQR_PS_MD3_Death2 The death nb. 2 sound)
     @value(EQR_PS_MD3_Death3 The death nb. 3 sound)
     @value(EQR_PS_MD3_Drown The drown sound)
     @value(EQR_PS_MD3_Fall1 The fall nb. 1 sound)
     @value(EQR_PS_MD3_Falling1 The falling nb. 1 sound)
     @value(EQR_PS_MD3_Gasp The gasp sound)
     @value(EQR_PS_MD3_Jump1 The jump nb. 1 sound)
     @value(EQR_PS_MD3_Pain25_1 The pain nb. 1 sound)
     @value(EQR_PS_MD3_Pain50_1 The pain nb. 2 sound)
     @value(EQR_PS_MD3_Pain75_1 The pain nb. 3 sound)
     @value(EQR_PS_MD3_Pain100_1 The pain nb. 4 sound)
     @value(EQR_PS_MD3_Taunt The taunt sound)
    }
    {$ENDREGION}
    EQRMD3PackageSound =
    (
        EQR_PS_MD3_Death1 = 0,
        EQR_PS_MD3_Death2,
        EQR_PS_MD3_Death3,
        EQR_PS_MD3_Drown,
        EQR_PS_MD3_Fall1,
        EQR_PS_MD3_Falling1,
        EQR_PS_MD3_Gasp,
        EQR_PS_MD3_Jump1,
        EQR_PS_MD3_Pain25_1,
        EQR_PS_MD3_Pain50_1,
        EQR_PS_MD3_Pain75_1,
        EQR_PS_MD3_Pain100_1,
        EQR_PS_MD3_Taunt
    );

    {$REGION 'Documentation'}
    {**
     Called when the model should be unpacked
     @param(pGroup Group at which model belongs)
     @param(pPackage MD3 model package)
     @param(handled @bold([in, out]) If @true, model will be considered as unpacked and no further
                                     operation will be done. If @false, model will be unpacked using
                                     the standard algorithm)
     @return(@true on success, otherwise @false)
     @br @bold(NOTE) Be careful, newly added files in memory dir may conflict with unpacked files
                     while standard algorithm is applied if handled is set to @false
    }
    {$ENDREGION}
    TQRUnpackMD3ModelEvent = function(const pGroup: TQRModelGroup;
                                          pPackage: TStream;
                                              pDir: TQRMemoryDir;
                                       var handled: Boolean): Boolean of object;

    {$REGION 'Documentation'}
    {**
     Job to load a MD3 model from a package (*.pk3 or .zip)
     @br @bold(NOTE) The role of a job is to do something in a thread. A Job is basically executed
                     by a worker. A MD3 job is designed to load all the files composing the MD3
                     model, and provides the data to be used by the group
     @br @bold(NOTE) Some zip archives may be detected as valid but fails while stream is extracted,
                     by returning an incoherent stream content (no error is shown when this happen).
                     This seems to be a limitation of the zip library provided with the Embarcadero
                     Delphi compiler (tested with XE7 and earlier), and happen sometimes with some
                     packages created with old zippers, e.g. when RequiredVersion is set to 10 and
                     CompressionMethod is set to 0 in the returned TZipHeader record. The solution
                     for now is to extract and recreate the package using a recent zipper
    }
    {$ENDREGION}
    TQRLoadMD3PackageJob = class(TQRLoadMD3MemoryDirJob)
        private
            m_pPackage:                TStream;
            m_pIcon:                   TBitmap;
            m_ShaderFileName:          TFileName;
            m_ExternalUnpackSucceeded: Boolean;
            m_ExternalUnpackHandled:   Boolean;
            m_fOnUnpackModel:          TQRUnpackMD3ModelEvent;

        protected
            {$REGION 'Documentation'}
            {**
             Called when an input ZIP stream should be opened
             @param(pSender Event sender)
             @param(pStream @bold([in, out]) ZIP stream to open, opened ZIP stream after function ends)
            }
            {$ENDREGION}
            procedure OnOpenInputZipStream(pSender: TObject; var pStream: TStream); virtual;

            {$REGION 'Documentation'}
            {**
             Called when a ZIP stream should be created
             @param(pSender Event sender)
             @param(pStream @bold([in, out]) ZIP stream to create, created ZIP stream after function
                                             ends)
             @param(pItem ZIP item for which stream should be created)
            }
            {$ENDREGION}
            procedure OnCreateOutZipStream(pSender: TObject;
                                       var pStream: TStream;
                                             pItem: TFullZipFileEntry); virtual;

            {$REGION 'Documentation'}
            {**
             Called after a ZIP stream was populated
             @param(pSender Event sender)
             @param(pStream @bold([in, out]) stream containig the ZIP data)
             @param(pItem ZIP item for which stream should be created)
            }
            {$ENDREGION}
            procedure OnDoneOutZipStream(pSender: TObject;
                                     var pStream: TStream;
                                           pItem: TFullZipFileEntry); virtual;

            {$REGION 'Documentation'}
            {**
             Unpacks model package and prepare memory directory
             @return(@true on succes, otherwise @false)
            }
            {$ENDREGION}
            function Unpack: Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Gets icon
             @return(Icon, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetIcon: TBitmap; virtual;

            {$REGION 'Documentation'}
            {**
             Gets shader file
             @return(Shader file, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetShader: TStream; virtual;

            {$REGION 'Documentation'}
            {**
             Gets sound
             @param(Index Sound index)
             @return(Sound file, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetSound(index: EQRMD3PackageSound): TStream; virtual;

            {$REGION 'Documentation'}
            {**
             Called when model is about to be unpacked externally
            }
            {$ENDREGION}
            procedure OnUnpackModelExternally; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Construction
             @param(pGroup Group that owns the job)
             @param(pPackage Stream containing package model to load)
             @param(prefixKeyword Model templates prefix keyword)
             @param(pColor Model color)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(fOnLoadTexture Load texture callback function)
             @param(team Team at which the model belongs (if available))
             @param(customTeamName Custom team name, in case the team is custom)
             @raises(Exception if the group is not defined)
             @br @bold(NOTE) Package stream will be deleted internally, do not try to delete it from
                             outside
            }
            {$ENDREGION}
            constructor Create(pGroup: TQRModelGroup;
                       const pPackage: TStream;
                  const prefixKeyword: UnicodeString;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent;
                                 team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                       customTeamName: UnicodeString = ''); reintroduce;

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
             Gets the icon, @nil if no icon available
            }
            {$ENDREGION}
            property Icon: TBitmap read GetIcon;

            {$REGION 'Documentation'}
            {**
             Gets the shader, @nil if no shader available
            }
            {$ENDREGION}
            property Shader: TStream read GetShader;

            {$REGION 'Documentation'}
            {**
             Gets the sound, @nil if no sound available
            }
            {$ENDREGION}
            property Sound[index: EQRMD3PackageSound]: TStream read GetSound;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnUnpackModel event
            }
            {$ENDREGION}
            property OnUnpackModel: TQRUnpackMD3ModelEvent read m_fOnUnpackModel write m_fOnUnpackModel;
    end;

    TQRMD3ModelPostponedGestures = TDictionary<UnicodeString, EQRMD3AnimationGesture>;

    {$REGION 'Documentation'}
    {**
     MD3 model group, contains all the items and functions needed to manage a complete MD3 model
    }
    {$ENDREGION}
    TQRMD3Group = class(TQRFramedModelGroup)
        private
            m_PrefixKeyword:      UnicodeString;
            m_pJob:               TQRMD3Job;
            m_pPostponedGestures: TQRMD3ModelPostponedGestures;

        protected
            {$REGION 'Documentation'}
            {**
             Animates model
             @param(elapsedTime Elapsed time since last rendering)
             @param(pItem Model item containing sub-model to animate)
            }
            {$ENDREGION}
            procedure AnimateModel(elapsedTime: Double; pItem: TQRMD3ModelItem); virtual;

            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh
             @param(pItem Sub-model item to get from)
             @param(index Mesh index to calculate and get)
             @param(mesh @bold([out]) Mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMesh(const pItem: TQRMD3ModelItem;
                                           index: NativeUInt;
                                        out mesh: TQRMesh); virtual;

            {$REGION 'Documentation'}
            {**
             Gets dynamic mesh, from cache if available, otherwise calculates and caches it
             @param(pItem Sub-model item to get from)
             @param(index Mesh index to calculate and get)
             @param(mesh @bold([out]) Mesh)
             @param(pTree @bold([out]) Aligned-axis bounding box tree matching with mesh)
            }
            {$ENDREGION}
            procedure GetDynamicMeshUseCache(const pItem: TQRMD3ModelItem;
                                                   index: NativeUInt;
                                               out pMesh: PQRMesh;
                                               out pTree: TQRAABBTree); virtual;

            {$REGION 'Documentation'}
            {**
             Draws dynamic model
             @param(pItem Sub-model item to draw)
             @param(matrix Sub-model matrix)
            }
            {$ENDREGION}
            procedure DrawDynamicModel(const pItem: TQRMD3ModelItem;
                                      const matrix: TQRMatrix4x4); virtual;

            {$REGION 'Documentation'}
            {**
             Draws cached model
             @param(pItem Sub-model item to draw)
             @param(matrix Sub-model matrix)
            }
            {$ENDREGION}
            procedure DrawCachedModel(const pItem: TQRMD3ModelItem;
                                     const matrix: TQRMatrix4x4); virtual;

            {$REGION 'Documentation'}
            {**
             Draws the sub-model mesh
             @param(pItem Sub-model item to draw)
             @param(matrix Sub-model matrix)
            }
            {$ENDREGION}
            procedure DrawMesh(const pItem: TQRMD3ModelItem; const matrix: TQRMatrix4x4); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the memory dir containing model files
             @return(Memory dir, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetMemoryDir: TQRMemoryDir; virtual;

            {$REGION 'Documentation'}
            {**
             Gets icon
             @return(Icon, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetIcon: TBitmap;

            {$REGION 'Documentation'}
            {**
             Gets shader file
             @return(Shader file, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetShader: TStream;

            {$REGION 'Documentation'}
            {**
             Gets sound
             @param(index Sound index)
             @return(Sound file, @nil if not found or on error)
            }
            {$ENDREGION}
            function GetSound(index: EQRMD3PackageSound): TStream;

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
             @param(pInfo Group info)
             @param(pColor Model color)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) A standard md3 model is commonly composed of 3 sub-models, named
                             "lower", "upper" and "head". Additionaly the model may contain a
                             "weapon" sub-model, that should be declared explicitely in the
                             sub-models list (to addd to the info structure Prefixes list)
             @br @bold(NOTE) A MD3 model is generally composed by the following files:
                             @br - a set of .md3 files that contains the various sub-model parts
                             @br - a set of .skin files, matching with each sub-models
                             @br - a set of texture files, matching with each sub-models, can be of
                                   any type: bmp, jpg, pcx, ...
                             @br - an animation file, that describes all the animations the model can
                                   execute, composed as follow:
                                   @longcode(
                                             sex m/f
                                             [frame start] [frame count] [frame loop] [fps]
                                             0             39            0            20    // MODEL_WALK
                                             ...
                                             )
                             @br
                             Inside the dir, each md3 model can be identified by a group name, e.g.
                             to load the Lara model, the following files may be found in dir:
                             @br - lara_head.md3
                             @br - lara_head.skin
                             @br - lara_head.jpg
                             @br - lara_lower.md3
                             @br - lara_lower.skin
                             @br - lara_lower.jpg
                             @br - ...
                             @br - lara_animations.cfg
                             @br
                             The way the names are composed may vary depending of the choices made
                             by the model creator, for this reason the info structure provides
                             templates for file names, that may be configured as needed
            }
            {$ENDREGION}
            function Load(const dir: UnicodeString;
                        const pInfo: TQRMD3GroupInfo;
                       const pColor: TQRColor;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from memory dir
             @param(pDir Memory dir containing all model streams to load)
             @param(pInfo Group info)
             @param(pColor Model color)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Memory dir will be deleted internally, do not try to delete it from outside
             @br @bold(NOTE) A standard md3 model is commonly composed of 3 sub-models, named
                             "lower", "upper" and "head". Additionaly the model may contain a
                             "weapon" sub-model, that should be declared explicitely in the
                             sub-models list (to addd to the info structure Prefixes list)
             @br @bold(NOTE) A MD3 model is generally composed by the following files:
                             @br - a set of .md3 files that contains the various sub-model parts
                             @br - a set of .skin files, matching with each sub-models
                             @br - a set of texture files, matching with each sub-models, can be of
                                   any type: bmp, jpg, pcx, ...
                             @br - an animation file, that describes all the animations the model can
                                   execute, composed as follow:
                                   @longcode(
                                             sex m/f
                                             [frame start] [frame count] [frame loop] [fps]
                                             0             39            0            20    // MODEL_WALK
                                             ...
                                             )
                             @br
                             Inside the dir, each md3 model can be identified by a group name, e.g.
                             to load the Lara model, the following files may be found in dir:
                             @br - lara_head.md3
                             @br - lara_head.skin
                             @br - lara_head.jpg
                             @br - lara_lower.md3
                             @br - lara_lower.skin
                             @br - lara_lower.jpg
                             @br - ...
                             @br - lara_animations.cfg
                             @br
                             The way the names are composed may vary depending of the choices made
                             by the model creator, for this reason the info structure provides
                             templates for file names, that may be configured as needed
            }
            {$ENDREGION}
            function Load(const pDir: TQRMemoryDir;
                         const pInfo: TQRMD3GroupInfo;
                        const pColor: TQRColor;
                              rhToLh: Boolean;
                        modelOptions: TQRModelOptions;
                  framedModelOptions: TQRFramedModelOptions): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from package (.pk3 or .zip) file
             @param(fileName Model package file name to load)
             @param(pColor Model color)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(team Team at which the model belongs (if available))
             @param(customTeamName Custom team name, in case the team is custom)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Load(const fileName: TFileName;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                          customTeamName: UnicodeString = ''): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Loads group from package (.pk3 or .zip) stream
             @param(pPackage Stream containing model package to load)
             @param(pColor Model color)
             @param(rhToLh If @true, right hand coordinates will be transformed to left hand)
             @param(modelOptions Model options to apply)
             @param(framedModelOptions Framed model options to apply)
             @param(team Team at which the model belongs (if available))
             @param(customTeamName Custom team name, in case the team is custom)
             @return(@true on success, otherwise @false)
             @br @bold(NOTE) Package stream will be deleted internally, do not try to delete it from
                             outside
            }
            {$ENDREGION}
            function Load(const pPackage: TStream;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                          customTeamName: UnicodeString = ''): Boolean; overload; virtual;

            {$REGION 'Documentation'}
            {**
             Selects animation to run for a sub-model
             @param(name Sub-model name)
             @param(gesture Animation gesture to execute)
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function SetAnimation(const name: UnicodeString;
                                     gesture: EQRMD3AnimationGesture): Boolean; virtual;

            {$REGION 'Documentation'}
            {**
             Queries the job status
             @return(Job status)
            }
            {$ENDREGION}
            function QueryJobStatus: TQRModelJobStatus; override;

            {$REGION 'Documentation'}
            {**
             Draws the group
             @param(elapsedTime Elapsed time since last draw)
            }
            {$ENDREGION}
            procedure Draw(const elapsedTime: Double); override;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the memory directory, @nil if not available
            }
            {$ENDREGION}
            property MemoryDir: TQRMemoryDir read GetMemoryDir;

            {$REGION 'Documentation'}
            {**
             Gets the icon, @nil if not available
            }
            {$ENDREGION}
            property Icon: TBitmap read GetIcon;

            {$REGION 'Documentation'}
            {**
             Gets the shader, @nil if not available
            }
            {$ENDREGION}
            property Shader: TStream read GetShader;

            {$REGION 'Documentation'}
            {**
             Gets the sound at index, @nil if not available
            }
            {$ENDREGION}
            property Sound[index: EQRMD3PackageSound]: TStream read GetSound;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMD3Helper
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Helper.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Helper.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
class function TQRMD3Helper.BuildName(const templateName,
                                           prefixKeyword,
                                                  prefix: UnicodeString): UnicodeString;
begin
    // no template to build from?
    if (Length(templateName) = 0) then
        Exit(prefix);

    // build name from template
    Result := UnicodeString(StringReplace(AnsiString(templateName),
                                          AnsiString(prefixKeyword),
                                          AnsiString(prefix),
                                          []));
end;
//--------------------------------------------------------------------------------------------------
class function TQRMD3Helper.BuildTagName(const itemName: UnicodeString): UnicodeString;
var
    name: UnicodeString;
begin
    name := LowerCase(itemName);

    if (name = 'head') then
        Result := 'tag_head'
    else
    if (name = 'upper') then
        Result := 'tag_torso'
    else
        Result := '';
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3AnimCfgFile
//--------------------------------------------------------------------------------------------------
constructor TQRMD3AnimCfgFile.Create;
begin
    inherited Create;

    m_Gender         := EQR_GN_MD3_Unknown;
    m_StartLine      := 0;
    m_CurLine        := 0;
    m_ReadGender     := False;
    m_ReadHeadOffset := False;
    m_ReadFootSteps  := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3AnimCfgFile.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean;
begin
    m_ReadGender     := False;
    m_ReadHeadOffset := False;
    m_ReadFootSteps  := False;

    Result := inherited OnParseLine(line, lineNb);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.ParseWord(const word: UnicodeString; lineNb: NativeUInt): Boolean;
var
    index:   NativeUInt;
    gesture: NativeInt;
    c:       WideChar;
begin
    // search word meaning
    if (word = 'sex') then
        // gender indicator
        m_ReadGender := True
    else
    if (m_ReadGender) then
    begin
        // if gender is currently reading, m means male, and f means female. Another word is an error
        if (word = 'm') then
            m_Gender := EQR_GN_MD3_Male
        else
        if (word = 'f') then
            m_Gender := EQR_GN_MD3_Female
        else
            Exit(False);
    end
    else
    if (word = 'headoffset') then
        // head offset indicator
        m_ReadHeadOffset := True
    else
    if (m_ReadHeadOffset) then
    begin
        // is word empty?
        if (Length(word) = 0) then
            Exit(False);

        // by default, each line contains 4 numeric values, that describes the animation
        for c in word do
            if ((c <> '\0') and (not TQRStringHelper.IsNumeric(c, False))) then
                Exit(False);

        // search for head offset value to set
        case (Column) of
            0: m_HeadOffset.m_UnknownOffset1 := StrToInt(AnsiString(word));
            1: m_HeadOffset.m_UnknownOffset2 := StrToInt(AnsiString(word));
            2: m_HeadOffset.m_UnknownOffset3 := StrToInt(AnsiString(word));
        else
            Exit(False);
        end;

        IncColumn;
    end
    else
    if (word = 'footsteps') then
        // foot steps indicator
        m_ReadFootSteps := True
    else
    if (m_ReadFootSteps) then
    begin
        if (word = 'boot') then
            m_FootSteps.m_Mode := EQR_FS_MD3_Boot;
    end
    else
    begin
        // is word empty?
        if (Length(word) = 0) then
            Exit(False);

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

        // get current item index
        index := GetItemCount;

        // first animation value?
        if (Column = 0) then
        begin
            // get animation gesture (each line pos in file matchs with the IEGesture enumerator)
            gesture := (m_CurLine - m_StartLine);

            // invalid type?
            if (gesture >= NativeInt(EQR_AG_MD3_Max_Animations)) then
                Exit(False);

            // create and populate new item, and add it to list
            SetItemCount(index + 1);
            Items[index].m_Gesture := gesture;

            Inc(m_CurLine);
        end
        else
            Dec(index);

        // search for animation item value to set
        case (Column) of
            0: Items[index].m_StartFrame      := StrToInt(AnsiString(word));
            1: Items[index].m_FrameCount      := StrToInt(AnsiString(word));
            2: Items[index].m_LoopingFrames   := StrToInt(AnsiString(word));
            3: Items[index].m_FramesPerSecond := StrToInt(AnsiString(word));
        else
            Exit(False);
        end;

        IncColumn;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3AnimCfgFile.Clear;
begin
    inherited Clear;

    // clear memory
    FillChar(m_HeadOffset, SizeOf(m_HeadOffset), 0);
    FillChar(m_FootSteps,  SizeOf(m_FootSteps),  0);

    // reset values
    m_Gender         := EQR_GN_MD3_Unknown;
    m_StartLine      := 0;
    m_CurLine        := 0;
    m_ReadGender     := False;
    m_ReadHeadOffset := False;
    m_ReadFootSteps  := False;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.GetGender: EQRMD3Gender;
begin
    Result := m_Gender;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.GetHeadOffset: PQRMD3AnimCfgFileHeadOffset;
begin
    Result := @m_HeadOffset;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.GetFootSteps: PQRMD3AnimCfgFileFootSteps;
begin
    Result := @m_FootSteps;
end;
//--------------------------------------------------------------------------------------------------
class function TQRMD3AnimCfgFile.GestureToStr(gesture: EQRMD3AnimationGesture): UnicodeString;
begin
    case (gesture) of
        EQR_AG_MD3_Both_Death1:         Result := 'EQR_AG_MD3_Both_Death1';
        EQR_AG_MD3_Both_Dead1:          Result := 'EQR_AG_MD3_Both_Dead1';
        EQR_AG_MD3_Both_Death2:         Result := 'EQR_AG_MD3_Both_Death2';
        EQR_AG_MD3_Both_Dead2:          Result := 'EQR_AG_MD3_Both_Dead2';
        EQR_AG_MD3_Both_Death3:         Result := 'EQR_AG_MD3_Both_Death3';
        EQR_AG_MD3_Both_Dead3:          Result := 'EQR_AG_MD3_Both_Dead3';
        EQR_AG_MD3_Torso_Gesture:       Result := 'EQR_AG_MD3_Torso_Gesture';
        EQR_AG_MD3_Torso_Attack:        Result := 'EQR_AG_MD3_Torso_Attack';
        EQR_AG_MD3_Torso_Attack2:       Result := 'EQR_AG_MD3_Torso_Attack2';
        EQR_AG_MD3_Torso_Drop:          Result := 'EQR_AG_MD3_Torso_Drop';
        EQR_AG_MD3_Torso_Raise:         Result := 'EQR_AG_MD3_Torso_Raise';
        EQR_AG_MD3_Torso_Stand:         Result := 'EQR_AG_MD3_Torso_Stand';
        EQR_AG_MD3_Torso_Stand2:        Result := 'EQR_AG_MD3_Torso_Stand2';
        EQR_AG_MD3_Legs_Walk_Crouching: Result := 'EQR_AG_MD3_Legs_Walk_Crouching';
        EQR_AG_MD3_Legs_Walk:           Result := 'EQR_AG_MD3_Legs_Walk';
        EQR_AG_MD3_Legs_Run:            Result := 'EQR_AG_MD3_Legs_Run';
        EQR_AG_MD3_Legs_Back:           Result := 'EQR_AG_MD3_Legs_Back';
        EQR_AG_MD3_Legs_Swim:           Result := 'EQR_AG_MD3_Legs_Swim';
        EQR_AG_MD3_Legs_Jump:           Result := 'EQR_AG_MD3_Legs_Jump';
        EQR_AG_MD3_Legs_Land:           Result := 'EQR_AG_MD3_Legs_Land';
        EQR_AG_MD3_Legs_Jump_Back:      Result := 'EQR_AG_MD3_Legs_Jump_Back';
        EQR_AG_MD3_Legs_Land_Back:      Result := 'EQR_AG_MD3_Legs_Land_Back';
        EQR_AG_MD3_Legs_Idle:           Result := 'EQR_AG_MD3_Legs_Idle';
        EQR_AG_MD3_Legs_Idle_Crouching: Result := 'EQR_AG_MD3_Legs_Idle_Crouching';
        EQR_AG_MD3_Legs_Turn:           Result := 'EQR_AG_MD3_Legs_Turn';
        EQR_AG_MD3_Max_Animations:      Result := 'EQR_AG_MD3_Max_Animations';
    else
        raise Exception.Create('Unknown animation type');
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Skin
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Skin.Create;
begin
    inherited Create;

    m_PathTable := TQRMD3PathTable.Create;
    m_LinkKeys  := TQRMD3LinkKeys.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Skin.Destroy;
begin
    // clear memory
    m_PathTable.Free;
    m_LinkKeys.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Skin.OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean;
var
    name, path: UnicodeString;
    c:          WideChar;
    readPath:   Boolean;
begin
    // no line to parse?
    if (Length(line) = 0) then
        Exit(True);

    readPath := False;

    // iterate through line chars
    for c in line do
    begin
        // dispatch char
        case (c) of
            ',':
            begin
                // found separator, from now path should be read
                readPath := True;
                continue;
            end
        else
            // is reading path?
            if (readPath) then
            begin
                // add char to path
                path := path + c;
                continue;
            end;

            // add char to name
            name := name + c;
        end;
    end;

    // empty name?
    if (Length(name) = 0) then
        Exit(False);

    // no path?
    if (Length(path) = 0) then
    begin
        // in this case, the line contains a link key. Check if the name key already exists
        if (m_LinkKeys.IndexOf(AnsiString(name)) <> -1) then
            Exit(False);

        // add link key to table
        m_LinkKeys.Add(AnsiString(name));

        Exit(True);
    end;

    // name already exists in path table?
    if (m_PathTable.ContainsKey(name)) then
        Exit(False);

    // add name and path in table
    m_PathTable.Add(name, path);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Skin.Clear;
begin
    m_PathTable.Clear;
    m_LinkKeys.Clear;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Skin.GetPath(const name: UnicodeString): UnicodeString;
begin
    // name exists in table?
    if (not m_PathTable.ContainsKey(name)) then
        Exit('');

    Result := m_PathTable[name];
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3AnimationItem
//--------------------------------------------------------------------------------------------------
constructor TQRMD3AnimationItem.Create;
begin
    inherited Create;

    m_StartFrame      := 0;
    m_EndFrame        := 0;
    m_FramesPerSecond := 0;
    m_Loop            := False;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRMD3AnimationItem.Create(startFrame, endFrame, fps: NativeUInt; loop: Boolean);
begin
    inherited Create;

    m_StartFrame      := startFrame;
    m_EndFrame        := endFrame;
    m_FramesPerSecond := fps;
    m_Loop            := loop;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3AnimationItem.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3ModelLink
//--------------------------------------------------------------------------------------------------
constructor TQRMD3ModelLink.Create;
begin
    inherited Create;

    m_TagIndex := 0;
    m_pItem    := nil;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRMD3ModelLink.Create(tagIndex: NativeUInt; const pItem: Pointer);
begin
    inherited Create;

    m_TagIndex := tagIndex;
    m_pItem    := pItem;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3ModelLink.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3ModelItem
//--------------------------------------------------------------------------------------------------
constructor TQRMD3ModelItem.Create;
begin
    inherited Create;

    // create local variables
    m_pModel      := TQRMD3Model.Create;
    m_pSkin       := TQRMD3Skin.Create;
    m_pAnimations := TQRMD3AnimationDictionary.Create;
    m_pAnimation  := TQRFramedModelAnimation.Create;
    m_Gesture     := EQR_AG_MD3_Max_Animations;
    m_CacheIndex  := 0;
    m_RhToLh      := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3ModelItem.Destroy;
var
    animItem: TPair<EQRMD3AnimationGesture, TQRMD3AnimationItem>;
    pTexture: TQRTexture;
    pLink:    TQRMD3ModelLink;
begin
    // clear animations
    for animItem in m_pAnimations do
        animItem.Value.Free;

    m_pAnimations.Free;

    // clear textures
    for pTexture in m_Textures do
        pTexture.Free;

    SetLength(m_Textures, 0);

    // clear links from
    for pLink in m_LinksFrom do
        pLink.Free;

    SetLength(m_LinksFrom, 0);

    // clear links to
    for pLink in m_LinksTo do
        pLink.Free;

    SetLength(m_LinksTo, 0);

    // clear memory
    m_pModel.Free;
    m_pSkin.Free;
    m_pAnimation.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3ModelItem.AddLink(var links: TQRMD3ModelLinks;
                              const pTarget: TQRMD3ModelItem;
                                   tagIndex: NativeUInt);
var
    pLink: TQRMD3ModelLink;
    index: NativeUInt;
begin
    // iterate through existing links
    for pLink in links do
        // link already added?
        if (pLink.m_pItem = pTarget) then
            Exit;

    // add link
    index := Length(links);
    SetLength(links, index + 1);
    links[index] := TQRMD3ModelLink.Create(tagIndex, pTarget);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetAnimation(gesture: EQRMD3AnimationGesture): TQRMD3AnimationItem;
begin
    if (not m_pAnimations.ContainsKey(gesture)) then
        Exit(nil);

    Result := m_pAnimations[gesture];
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3ModelItem.SetAnimation(gesture: EQRMD3AnimationGesture;
                              const pAnimation: TQRMD3AnimationItem);
begin
    if (m_pAnimations.ContainsKey(gesture)) then
    begin
        m_pAnimations[gesture].Free;
        m_pAnimations[gesture] := pAnimation;
        Exit;
    end;

    m_pAnimations.Add(gesture, pAnimation);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetTexture(index: NativeInt): TQRTexture;
begin
    if (index >= Length(m_Textures)) then
        Exit(nil);

    Result := m_Textures[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinkFrom(index: NativeInt): TQRMD3ModelLink;
begin
    if (index >= Length(m_LinksFrom)) then
        Exit(nil);

    Result := m_LinksFrom[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinkTo(index: NativeInt): TQRMD3ModelLink;
begin
    if (index >= Length(m_LinksTo)) then
        Exit(nil);

    Result := m_LinksTo[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetTextureCount: NativeInt;
begin
    Result := Length(m_Textures);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinksFromCount: NativeInt;
begin
    Result := Length(m_LinksFrom);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinksToCount: NativeInt;
begin
    Result := Length(m_LinksTo);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetAnimationCount: NativeInt;
begin
    Result := m_pAnimations.Count;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3ModelItem.AddTexture(const pTexture: TQRTexture);
var
    index: NativeUInt;
begin
    index := Length(m_Textures);
    SetLength(m_Textures, index + 1);
    m_Textures[index] := pTexture;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3ModelItem.AddLinkFrom(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem);
begin
    AddLink(m_LinksFrom, pTarget, tagIndex);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3ModelItem.AddLinkTo(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem);
begin
    AddLink(m_LinksTo, pTarget, tagIndex);
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3GroupInfo
//--------------------------------------------------------------------------------------------------
constructor TQRMD3GroupInfo.Create(createDefaultPrefixes: Boolean);
begin
    inherited Create;

    m_ModelTemplate := '%s';
    m_SkinTemplate  := '%s';
    m_AnimTemplate  := '%s';
    m_pPrefixes     := TStringList.Create;

    // do create default prefixes?
    if (createDefaultPrefixes) then
    begin
        m_pPrefixes.Add('lower');
        m_pPrefixes.Add('upper');
        m_pPrefixes.Add('head');
    end;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRMD3GroupInfo.Create(const pOther: TQRMD3GroupInfo);
begin
    inherited Create;

    // create local variables
    m_pPrefixes := TStringList.Create;

    // no other group info to copy from?
    if (not Assigned(pOther)) then
        Exit;

    // copy other group info content
    m_ModelTemplate := pOther.m_ModelTemplate;
    m_SkinTemplate  := pOther.m_SkinTemplate;
    m_AnimTemplate  := pOther.m_AnimTemplate;
    m_pPrefixes.Assign(pOther.m_pPrefixes);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3GroupInfo.Destroy;
begin
    // clear memory
    m_pPrefixes.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3GroupInfo.GetPrefix(index: NativeInt): UnicodeString;
begin
    // is index out of bounds?
    if (index >= m_pPrefixes.Count) then
        Exit('');

    Result := UnicodeString(m_pPrefixes[index]);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3GroupInfo.GetPrefixCount: NativeInt;
begin
    Result := m_pPrefixes.Count;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3GroupInfo.AddPrefix(const prefix: UnicodeString);
var
    index: Integer;
begin
    // prefix already exists in list?
    if (m_pPrefixes.Find(AnsiString(prefix), index)) then
        Exit;

    m_pPrefixes.Add(AnsiString(prefix));
end;
//--------------------------------------------------------------------------------------------------
// TQRMD3Job
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Job.Create(pGroup: TQRModelGroup;
                        const pInfo: TQRMD3GroupInfo;
                       const pColor: TQRColor;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions;
                     fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup, modelOptions);

    // create local variables
    m_TextureLoaded := False;
    m_IsCanceled    := False;

    // copy values needed to load the model
    m_pItemDictionary    :=  TQRMD3ItemDictionary.Create;
    m_pInfo              :=  TQRMD3GroupInfo.Create(pInfo);
    m_pColor             :=  TQRColor.Create(pColor);
    m_ItemIndex          := -1;
    m_MaxTexture         :=  100;
    m_FramedModelOptions :=  framedModelOptions;
    m_fOnLoadTexture     :=  fOnLoadTexture;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Job.Destroy;
var
    pItem: TQRMD3ModelItem;
begin
    m_pLock.Lock;

    try
        // clear items
        for pItem in m_Items do
            pItem.Free;

        SetLength(m_Items, 0);

        // clear memory
        m_pItemDictionary.Free;
        m_pInfo.Free;
        m_pColor.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.GetItem(index: NativeInt): TQRMD3ModelItem;
begin
    m_pLock.Lock;

    try
        if (index >= Length(m_Items)) then
            Exit(nil);

        Result := m_Items[index];
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.GetItem(const name: UnicodeString): TQRMD3ModelItem;
begin
    m_pLock.Lock;

    try
        if (not m_pItemDictionary.ContainsKey(name)) then
            Exit(nil);

        Result := m_pItemDictionary[name];
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.GetFramedModelOptions: TQRFramedModelOptions;
begin
    m_pLock.Lock;
    Result := m_FramedModelOptions;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Job.SetFramedModelOptions(options: TQRFramedModelOptions);
begin
    m_pLock.Lock;
    m_FramedModelOptions := options;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Job.LinkModel;
var
    itemCount, tagCount, targetTagCount, srcIndex, dstIndex, i, j, k, l, m: NativeUInt;
    linkName, targetName, srcTagName, dstTagName:                           UnicodeString;
    pParser, pTargetParser:                                                 TQRMD3Parser;
begin
    // get items count
    itemCount := Length(m_Items);

    if (itemCount = 0) then
        Exit;

    // iterate through items
    for i := 0 to itemCount - 1 do
    begin
        // item contains model?
        if (not Assigned(m_Items[i].m_pModel)) then
            continue;

        // get model parser
        pParser := m_Items[i].m_pModel.Parser;

        // found it?
        if (not Assigned(pParser)) then
            continue;

        // get model tag count
        tagCount := pParser.Header.m_TagCount;

        // no model tags
        if (tagCount = 0) then
            continue;

        // iterate through model tags
        for j := 0 to tagCount - 1 do
        begin
            // get tag name to link with
            linkName := UnicodeString(AnsiString(pParser.Tags[j].m_Name));

            // iterate throuugh remaining items
            for k := i + 1 to itemCount - 1 do
            begin
                // next item contains model
                if (not Assigned(m_Items[k].m_pModel)) then
                    continue;

                // get target model parser
                pTargetParser := m_Items[k].m_pModel.Parser;

                // found it?
                if (not Assigned(pTargetParser)) then
                    continue;

                // get target model tag count
                targetTagCount := pTargetParser.Header.m_TagCount;

                if (targetTagCount = 0) then
                    continue;

                // iterate through target tags
                for l := 0 to targetTagCount - 1 do
                begin
                    // get target link name
                    targetName := UnicodeString(AnsiString(pTargetParser.Tags[l].m_Name));

                    // found models to link?
                    if (linkName = targetName) then
                    begin
                        // build source and destination tag name to search
                        srcTagName := TQRMD3Helper.BuildTagName(m_Items[i].m_Name);
                        dstTagName := TQRMD3Helper.BuildTagName(m_Items[k].m_Name);

                        // set default source and destination tag indexes to link with
                        srcIndex := j;
                        dstIndex := l;

                        // search for source index by name. If not found, default index will be used
                        if (Length(srcTagName) > 0) then
                            for m := 0 to tagCount - 1 do
                                if (dstTagName = UnicodeString(AnsiString(pParser.Tags[m].m_Name))) then
                                begin
                                    srcIndex := m;
                                    break;
                                end;

                        // search for dest index by name. If not found, default index will be used
                        if (Length(dstTagName) > 0) then
                            for m := 0 to targetTagCount - 1 do
                                if (srcTagName = UnicodeString(AnsiString(pTargetParser.Tags[m].m_Name))) then
                                begin
                                    dstIndex := m;
                                    break;
                                end;

                        // link models
                        m_Items[i].AddLinkTo(srcIndex, m_Items[k]);
                        m_Items[k].AddLinkFrom(dstIndex, m_Items[i]);
                        break;
                    end;
                end;
            end;
        end;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.LinkSkin(const pSkin: TQRMD3Skin;
                              const dir: UnicodeString;
                            const pItem: TQRMD3ModelItem): Boolean;
var
    texturePos:            NativeInt;
    i, index:              NativeUInt;
    pParser:               TQRMD3Parser;
    meshName, texturePath: UnicodeString;
    meshNames:             array of UnicodeString;
    pathItem:              TPair<UnicodeString, UnicodeString>;
    found, textureLoaded:  Boolean;
begin
    // no item?
    if (not Assigned(pItem)) then
        Exit(False);

    // item contains no model?
    if (not Assigned(pItem.m_pModel)) then
        Exit(False);

    // get model parser
    pParser := pItem.m_pModel.Parser;

    // found it?
    if (not Assigned(pParser)) then
        Exit(False);

    // iterate through model meshes
    if (pParser.Header.m_MeshCount > 0) then
        for i := 0 to pParser.Header.m_MeshCount - 1 do
        begin
            // get mesh name
            meshName := UnicodeString(AnsiString(pParser.Meshes[i].m_Info.m_Name));

            // get texture path from skin file
            texturePath := pSkin.GetPath(meshName);

            // search for texture name position in line, from last char to first char
            texturePos := TQRStringHelper.LastPos('/', texturePath) - 1;

            // add mesh name to handled names list
            index := Length(meshNames);
            SetLength(meshNames, index + 1);
            meshNames[index] := meshName;

            // found it?
            if (texturePos = 0) then
                continue;

            m_pLock.Lock;

            try
                m_TextureName     := meshName;
                m_TextureFileName := System.Copy(texturePath,
                                                 texturePos + 2,
                                                 Length(texturePath) - (texturePos + 1));
            finally
                m_pLock.Unlock;
            end;

            // notify main interface that texture should be loaded, wait until function returns
            TThread.Synchronize(nil, OnLoadTexture);

            m_pLock.Lock;
            textureLoaded := m_TextureLoaded;
            m_pLock.Unlock;

            // failed to load texture?
            if (not textureLoaded) then
                Exit(False);
        end;

    // iterate through all skin path entries and load remaining textures
    if (Length(meshNames) > 0) then
        for pathItem in pItem.m_pSkin.m_PathTable do
        begin
            found := False;

            // search for already loaded textures
            for i := 0 to Length(meshNames) - 1 do
                if (meshNames[i] = pathItem.Key) then
                begin
                    found := True;
                    break;
                end;

            // texture was already loaded?
            if (found) then
                continue;

            // get texture path from skin file
            texturePath := pathItem.Value;

            // search for texture name position in line, from last char to first char
            texturePos := TQRStringHelper.LastPos('/', texturePath) - 1;

            // add mesh name to handled names list
            index := Length(meshNames);
            SetLength(meshNames, index + 1);
            meshNames[index] := meshName;

            // found it?
            if (texturePos = 0) then
                continue;

            m_pLock.Lock;

            try
                m_TextureName     := meshName;
                m_TextureFileName := System.Copy(texturePath,
                                                 texturePos + 2,
                                                 Length(texturePath) - (texturePos + 1));
            finally
                m_pLock.Unlock;
            end;

            // notify main interface that texture should be loaded, wait until function returns
            TThread.Synchronize(nil, OnLoadTexture);

            m_pLock.Lock;
            textureLoaded := m_TextureLoaded;
            m_pLock.Unlock;

            // failed to load texture?
            if (not textureLoaded) then
                Exit(False);
        end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.LinkAnimations(const pInfo: TQRMD3GroupInfo;
                            const pAnimations: TQRMD3AnimCfgFile): Boolean;
var
    animCount, lowerDelta, i, j: NativeInt;
    pItem, pStartItem, pEndItem: PQRModelAnimCfgItem;
    pAnimItem:                   TQRMD3AnimationItem;
begin
    animCount  := pAnimations.GetItemCount;
    lowerDelta := 0;

    if (animCount = 0) then
        Exit(True);

    // iterate through animations to link
    for i := 0 to animCount - 1 do
    begin
        // get animation item
        pItem := pAnimations.GetItem(i);

        // found it?
        if (not Assigned(pItem)) then
            continue;

        // search model part to which animation should be linked
        if ((pItem.m_Gesture >= Integer(EQR_AG_MD3_Both_Death1)) and
            (pItem.m_Gesture <= Integer(EQR_AG_MD3_Both_Dead3)))
        then
        begin
            // iterate through model parts
            for j := 0 to pInfo.m_pPrefixes.Count - 1 do
                // found upper or lower index?
                if ((pInfo.m_pPrefixes[j] = 'upper') or (pInfo.m_pPrefixes[j] = 'lower')) then
                begin
                    // create and populate new animation item
                    pAnimItem                   := TQRMD3AnimationItem.Create;
                    pAnimItem.m_StartFrame      := pItem.m_StartFrame;
                    pAnimItem.m_EndFrame        := pItem.m_StartFrame + (pItem.m_FrameCount - 1);
                    pAnimItem.m_Loop            := Boolean(pItem.m_LoopingFrames);
                    pAnimItem.m_FramesPerSecond := pItem.m_FramesPerSecond;

                    // add animation to model item
                    m_Items[j].Animations[EQRMD3AnimationGesture(pItem.m_Gesture)] := pAnimItem;
                end;
        end
        else
        if ((pItem.m_Gesture >= Integer(EQR_AG_MD3_Torso_Gesture)) and
            (pItem.m_Gesture <= Integer(EQR_AG_MD3_Torso_Stand2)))
        then
        begin
            // iterate through model parts
            for j := 0 to pInfo.m_pPrefixes.Count - 1 do
                // found upper index?
                if (pInfo.m_pPrefixes[j] = 'upper') then
                begin
                    // create and populate new animation item
                    pAnimItem                   := TQRMD3AnimationItem.Create;
                    pAnimItem.m_StartFrame      := pItem.m_StartFrame;
                    pAnimItem.m_EndFrame        := pItem.m_StartFrame + (pItem.m_FrameCount - 1);
                    pAnimItem.m_Loop            := Boolean(pItem.m_LoopingFrames);
                    pAnimItem.m_FramesPerSecond := pItem.m_FramesPerSecond;

                    // add animation to model item
                    m_Items[j].Animations[EQRMD3AnimationGesture(pItem.m_Gesture)] := pAnimItem;
                end;
        end
        else
        if ((pItem.m_Gesture >= Integer(EQR_AG_MD3_Legs_Walk_Crouching)) and
            (pItem.m_Gesture <= Integer(EQR_AG_MD3_Legs_Turn)))
        then
        begin
            // do calculate animation delta to apply to lower model part?
            if (lowerDelta = 0) then
            begin
                // get start and end animations from which delta should be calculated
                pStartItem := pAnimations.GetItem(Integer(EQR_AG_MD3_Torso_Gesture));
                pEndItem   := pAnimations.GetItem(Integer(EQR_AG_MD3_Legs_Walk_Crouching));

                // found them?
                if ((not Assigned(pStartItem)) or (not Assigned(pEndItem))) then
                    Exit(False);

                // calculate animation delta to apply to lower model part
                lowerDelta := pEndItem.m_StartFrame - pStartItem.m_StartFrame;
            end;

            // iterate through model parts
            for j := 0 to pInfo.m_pPrefixes.Count - 1 do
                // found lower index?
                if (pInfo.m_pPrefixes[j] = 'lower') then
                begin
                    // create and populate new animation item
                    pAnimItem                   := TQRMD3AnimationItem.Create;
                    pAnimItem.m_StartFrame      := pItem.m_StartFrame;
                    pAnimItem.m_EndFrame        := pItem.m_StartFrame + (pItem.m_FrameCount - 1);
                    pAnimItem.m_Loop            := Boolean(pItem.m_LoopingFrames);
                    pAnimItem.m_FramesPerSecond := pItem.m_FramesPerSecond;

                    // apply delta to animation values
                    Dec(pAnimItem.m_StartFrame, lowerDelta);
                    Dec(pAnimItem.m_EndFrame,   lowerDelta);

                    // add animation to model item
                    m_Items[j].Animations[EQRMD3AnimationGesture(pItem.m_Gesture)] := pAnimItem;
                end;
        end;
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Job.OnLoadTexture;
var
    max:                 NativeUInt;
    loadFirst, loadNext: Boolean;
    pModelTexture:       TQRTexture;
    pTexture:            Graphics.TBitmap;
    pItem:               TQRMD3ModelItem;
begin
    m_pLock.Lock;

    try
        m_TextureLoaded := False;

        if (GetStatus = EQR_JS_Canceled) then
            Exit;

        // get model item for which texture should be loaded
        pItem := GetItem(m_ItemIndex);

        // found it?
        if (not Assigned(pItem)) then
            Exit;

        loadFirst := True;
        loadNext  := True;
        max       := 0;

        // load next texture, until no more texture should be loaded
        repeat
            // by default don't load another texture after the current one will be loaded, because
            // in the MD3 standards, only one texture file belongs to model. However this value may
            // be modified externally by any user that handle the OnLoadTexture event, to load any
            // additional textures as required
            loadNext := False;

            // create a new model texture
            pModelTexture := TQRTexture.Create;

            try
                // notify that a texture is about to be loaded
                BeforeLoadTexture(pModelTexture, not loadFirst);

                // do load texture for the first time? (NOTE try to load only the first texture,
                // others are user defined textures)
                if (loadFirst) then
                begin
                    pTexture := Graphics.TBitmap.Create;

                    try
                        // load texture
                        if (LoadTexture(pModelTexture, pTexture)) then
                        begin
                            // notify that a texture is loading
                            if (Assigned(m_fOnLoadTexture)) then
                                if (not m_fOnLoadTexture(GetGroup,
                                                         pItem.m_pModel,
                                                         pTexture,
                                                         pModelTexture,
                                                         loadNext))
                                then
                                    Exit;
                        end
                        else
                        // notify that a texture is loading
                        if (Assigned(m_fOnLoadTexture)) then
                            if (not m_fOnLoadTexture(GetGroup,
                                                     pItem.m_pModel,
                                                     nil,
                                                     pModelTexture,
                                                     loadNext))
                            then
                                Exit;
                    finally
                        pTexture.Free;
                    end;

                    loadFirst := False;
                end
                else
                begin
                    // notify that a texture is loading
                    if (Assigned(m_fOnLoadTexture)) then
                        if (not m_fOnLoadTexture(GetGroup,
                                                 pItem.m_pModel,
                                                 nil,
                                                 pModelTexture,
                                                 loadNext))
                        then
                            Exit;
                end;

                // add texture to model item
                pItem.AddTexture(pModelTexture);
                pModelTexture := nil;
            finally
                pModelTexture.Free;
            end;

            Inc(max);

            // too many textures were loaded?
            if (max > m_MaxTexture) then
                raise Exception.Create('Too many textures were created');
        until (not loadNext);

        m_TextureLoaded := True;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Job.Cancel;
begin
    m_pLock.Lock;
    m_IsCanceled := True;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.IsCanceled: Boolean;
begin
    m_pLock.Lock;
    Result := m_IsCanceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMD3FileJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMD3FileJob.Create(pGroup: TQRModelGroup;
                                  const dir,
                              prefixKeyword: UnicodeString;
                                const pInfo: TQRMD3GroupInfo;
                               const pColor: TQRColor;
                               modelOptions: TQRModelOptions;
                         framedModelOptions: TQRFramedModelOptions;
                             fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup,
                     pInfo,
                     pColor,
                     modelOptions,
                     framedModelOptions,
                     fOnLoadTexture);

    // copy values needed to load the model
    m_Dir           := dir;
    m_PrefixKeyword := prefixKeyword;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMD3FileJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.LoadSkin(const fileName: TFileName;
                                       const pItem: TQRMD3ModelItem): Boolean;
begin
    // load skin file
    if (not pItem.m_pSkin.Load(fileName)) then
        Exit(False);

    Result := LinkSkin(pItem.m_pSkin,
                       TQRFileHelper.AppendDelimiter(UnicodeString(ExtractFileDir(fileName))),
                       pItem);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                       const fileName: TFileName): Boolean;
var
    pAnimations: TQRMD3AnimCfgFile;
begin
    pAnimations := TQRMD3AnimCfgFile.Create;

    try
        // load animation configuration file
        if (not pAnimations.Load(fileName)) then
            Exit(False);

        Result := LinkAnimations(pInfo, pAnimations);
    finally
        pAnimations.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3FileJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    // populate texture
    pTexture.Name := m_TextureName;
    pTexture.Dir  := m_Dir;

    // set file name only if texture is not a custom user defined texture
    if (custom) then
        pTexture.FileName := ''
    else
        pTexture.FileName := AnsiString(m_TextureFileName);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.LoadTexture(pTexture: TQRTexture;
                                        pBitmap: Graphics.TBitmap): Boolean;
var
    fileName:      TFileName;
    pFileStream:   TFileStream;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
        Exit(False);

    // build texture file name
    fileName := AnsiString(TQRFileHelper.AppendDelimiter(pTexture.Dir)) + pTexture.FileName;

    // check if texture file exists
    if (not FileExists(fileName)) then
        Exit(False);

    // load image file in a stream
    pFileStream := TFileStream.Create(fileName, fmOpenRead);

    try
        // found it?
        if (not Assigned(pFileStream)) then
            Exit(False);

        // load texture
        Result := TQRModelGroupHelper.LoadTexture(pFileStream,
                                                  UnicodeString(ExtractFileExt(pTexture.FileName)),
                                                  pBitmap);
    finally
        // clear memory
        pFileStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.Process: Boolean;
var
    vertexFormat:                                         TQRVertexFormat;
    pMesh:                                                PQRMesh;
    pTree:                                                TQRAABBTree;
    modelName, modelFileName, skinFileName, animFileName: TFileName;
    subModelGroupCount:                                   NativeInt;
    frameCount, cacheIndex, i, j:                         NativeUInt;
    progressStep, totalItemStep, totalStep:               Single;
    textureLoaded, doCreateCache:                         Boolean;
begin
    // if job was still loaded, don't reload it
    if (IsLoaded) then
        Exit(True);

    try
        Progress := 0.0;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in ModelOptions));

        // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
        // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
        // (except for optional files) begins with the same prefix, that is usually head, upper or lower
        subModelGroupCount := m_pInfo.m_pPrefixes.Count;

        // iterate through sub-models to load
        if (subModelGroupCount = 0) then
            Exit(False);

        m_pLock.Lock;

        try
            // initialize memory for sub-model items
            SetLength(m_Items, subModelGroupCount);
        finally
            m_pLock.Unlock;
        end;

        // calculate the total progress available for each item, plus one for the final part
        totalItemStep := 100.0 / (subModelGroupCount + 1);
        cacheIndex    := 0;

        // iterate through sub-models to create
        for i := 0 to subModelGroupCount - 1 do
        begin
            m_pLock.Lock;

            try
                // create new item. Doing that, a default model and skin will be created inside the item
                m_Items[i]        := TQRMD3ModelItem.Create;
                m_Items[i].m_Name := UnicodeString(m_pInfo.m_pPrefixes[i]);
                m_ItemIndex       := i;
            finally
                m_pLock.Unlock;
            end;

            // build model name
            modelName := TFileName(TQRMD3Helper.BuildName(m_pInfo.m_ModelTemplate,
                                                          m_PrefixKeyword,
                                                          UnicodeString(m_pInfo.m_pPrefixes[i])));

            // build model file name
            modelFileName := AnsiString(TQRFileHelper.AppendDelimiter(m_Dir)) + modelName + '.md3';

            // model file exists?
            if (not FileExists(modelFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 model file not exists - ' +
                                               modelFileName                  +
                                               ' - class name - '             +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // load md3 model
            if (not m_Items[i].m_pModel.Load(modelFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to load MD3 model - file name - ' +
                                               modelFileName                             +
                                               ' - class name - '                        +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // get mesh count
            frameCount := m_Items[i].m_pModel.GetMeshCount;

            // do create cache?
            if (doCreateCache) then
                // calculate step count
                totalStep := frameCount + 3.0
            else
                // get step count
                totalStep := 3.0;

            // calculate the value of a progress step
            progressStep := (totalItemStep / totalStep);

            // model is loaded, add one step to progress
            Progress := Progress + progressStep;

            // set model color
            m_Items[i].m_pModel.Color := m_pColor;

            // build skin file name
            skinFileName := AnsiString(TQRFileHelper.AppendDelimiter(m_Dir)           +
                                       TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                                              m_PrefixKeyword,
                                                              UnicodeString(m_pInfo.m_pPrefixes[i])) +
                                       '.skin');

            // skin file exists?
            if (not FileExists(skinFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 skin file not exists - ' +
                                               skinFileName                  +
                                               ' - class name - '            +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // load md3 skin
            if (not LoadSkin(skinFileName, m_Items[i])) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to load MD3 skin - file name - ' +
                                               skinFileName                             +
                                               ' - class name - '                       +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            m_pLock.Lock;

            try
                textureLoaded := m_TextureLoaded;
            finally
                m_pLock.Unlock;
            end;

            // skin is loaded, add one step to progress
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
            m_Items[i].m_pModel.VertexFormat := vertexFormat;

            // add newly created item to dictionary (to facilitate search later)
            m_pItemDictionary.AddOrSetValue(UnicodeString(modelName), m_Items[i]);

            // model is configured and default mesh is created, add one step to progress
            Progress := Progress + progressStep;

            // do not create cache?
            if (not doCreateCache) then
            begin
                // set cache index, in case model needs to be cached later
                m_Items[i].m_CacheIndex := cacheIndex;
                Inc(cacheIndex, frameCount);
                continue;
            end;

            // something to cache?
            if (frameCount > 0) then
            begin
                // keep index from where item frames will be added in cache
                m_Items[i].m_CacheIndex := cacheIndex;

                // iterate through frames to cache
                for j := 0 to frameCount - 1 do
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
                    if (not m_Items[i].m_pModel.GetMesh(j, pMesh^, pTree, IsCanceled)) then
                    begin
                        {$ifdef DEBUG}
                            TQRLogHelper.LogToCompiler('MD3 model frame creation failed or was canceled - name - ' +
                                                       modelFileName                                               +
                                                       ' - index - '                                               +
                                                       IntToStr(Int64(j))                                          +
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
                        SetMesh(m_Items[i].m_CacheIndex + j, pMesh);
                    except
                        Dispose(pMesh);
                    end;

                    // do ignore collisions?
                    if (not(EQR_MO_No_Collision in ModelOptions)) then
                        // add tree to cache, note that from now cache will take care of the pointer
                        try
                            SetTree(m_Items[i].m_CacheIndex + j, pTree);
                        except
                            pTree.Free;
                        end;

                    // update next available cache index position
                    Inc(cacheIndex);

                    // a new frame was cached, add one step to progress
                    Progress := Progress + progressStep;
                end;
            end;
        end;

        LinkModel;

        // calculate the value of a progress step
        progressStep := (totalItemStep / 2.0);

        // model was linked, add one step to progress
        Progress := Progress + progressStep;

        // build animation file name
        animFileName :=
                TFileName(TQRFileHelper.AppendDelimiter(m_Dir)           +
                          TQRMD3Helper.BuildName(m_pInfo.m_AnimTemplate,
                                                 m_PrefixKeyword,
                                                 'animation')            +
                          '.cfg');

        // open it
        if (not LoadAnimations(m_pInfo, animFileName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to load MD3 animation - file name - ' +
                                           animFileName                                  +
                                           ' - class name - '                            +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // model is fully loaded
        Progress := 100.0;
        IsLoaded := True;
        Result   := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMD3MemoryDirJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMD3MemoryDirJob.Create(pGroup: TQRModelGroup;
                                      const pDir: TQRMemoryDir;
                             const prefixKeyword: UnicodeString;
                                     const pInfo: TQRMD3GroupInfo;
                                    const pColor: TQRColor;
                                    modelOptions: TQRModelOptions;
                              framedModelOptions: TQRFramedModelOptions;
                                  fOnLoadTexture: TQRLoadMeshTextureEvent);
begin
    inherited Create(pGroup,
                     pInfo,
                     pColor,
                     modelOptions,
                     framedModelOptions,
                     fOnLoadTexture);

    // copy values needed to load the model
    m_pDir          := pDir;
    m_PrefixKeyword := prefixKeyword;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMD3MemoryDirJob.Destroy;
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
function TQRLoadMD3MemoryDirJob.LoadSkin(pStream: TStream; const pItem: TQRMD3ModelItem): Boolean;
begin
    // load skin file
    if (not pItem.m_pSkin.Load(pStream, pStream.Size)) then
        Exit(False);

    Result := LinkSkin(pItem.m_pSkin, '', pItem);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                                   pStream: TStream): Boolean;
var
    pAnimations: TQRMD3AnimCfgFile;
begin
    pAnimations := TQRMD3AnimCfgFile.Create;

    try
        // load animation configuration file
        if (not pAnimations.Load(pStream, pStream.Size)) then
            Exit(False);

        Result := LinkAnimations(pInfo, pAnimations);
    finally
        pAnimations.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3MemoryDirJob.BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean);
begin
    // populate texture
    pTexture.Name     := m_TextureName;
    pTexture.Dir      := '';

    // set file name only if texture is not a custom user defined texture
    if (custom) then
        pTexture.FileName := ''
    else
        pTexture.FileName := AnsiString(m_TextureFileName);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.LoadTexture(pTexture: TQRTexture;
                                             pBitmap: Graphics.TBitmap): Boolean;
var
    pImageStream: TStream;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
        Exit(False);

    // file exists in memory dir?
    if (not m_pDir.FileExists(pTexture.FileName)) then
        Exit(False);

    // get image stream
    pImageStream := m_pDir.GetFile(pTexture.FileName);

    // found it?
    if (not Assigned(pImageStream)) then
        Exit(False);

    pImageStream.Position := 0;

    // load texture
    Result := TQRModelGroupHelper.LoadTexture(pImageStream,
                                              UnicodeString(ExtractFileExt(pTexture.FileName)),
                                              pBitmap);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.GetMemoryDir: TQRMemoryDir;
begin
    m_pLock.Lock;
    Result := m_pDir;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.Process: Boolean;
var
    pModelStream, pSkinStream, pAnimCfgStream:            TStream;
    vertexFormat:                                         TQRVertexFormat;
    pMesh:                                                PQRMesh;
    pTree:                                                TQRAABBTree;
    modelName, modelFileName, skinFileName, animFileName: TFileName;
    subModelGroupCount:                                   NativeInt;
    frameCount, cacheIndex, i, j:                         NativeUInt;
    progressStep, totalItemStep, totalStep:               Single;
    textureLoaded, doCreateCache:                         Boolean;
begin
    // if job was still loaded, don't reload it
    if (IsLoaded) then
        Exit(True);

    try
        Progress := 0.0;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in ModelOptions));

        // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
        // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
        // (except for optional files) begins with the same prefix, that is usually head, upper or lower
        subModelGroupCount := m_pInfo.m_pPrefixes.Count;

        // iterate through sub-models to load
        if (subModelGroupCount = 0) then
            Exit(False);

        m_pLock.Lock;

        try
            // initialize memory for sub-model items
            SetLength(m_Items, subModelGroupCount);
        finally
            m_pLock.Unlock;
        end;

        // calculate the total progress available for each item, plus one for the final part
        totalItemStep := 100.0 / (subModelGroupCount + 1);
        cacheIndex    := 0;

        // iterate through sub-models to create
        for i := 0 to subModelGroupCount - 1 do
        begin
            m_pLock.Lock;

            try
                // create new item. Doing that, a default model and skin will be created inside the item
                m_Items[i]        := TQRMD3ModelItem.Create;
                m_Items[i].m_Name := UnicodeString(m_pInfo.m_pPrefixes[i]);
                m_ItemIndex       := i;
            finally
                m_pLock.Unlock;
            end;

            // build model name
            modelName := TFileName(TQRMD3Helper.BuildName(m_pInfo.m_ModelTemplate,
                                                          m_PrefixKeyword,
                                                          UnicodeString(m_pInfo.m_pPrefixes[i])));

            // build model file name
            modelFileName := modelName + '.md3';

            // model file exists?
            if (not m_pDir.FileExists(modelFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 model stream does not exist - ' +
                                               modelFileName                        +
                                               ' - class name - '                   +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // get stream containing md3 data
            pModelStream := m_pDir.GetFile(modelFileName);

            // found it?
            if (not Assigned(pModelStream)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to get MD3 model stream - ' +
                                               modelName                           +
                                               ' - class name - '                  +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // load md3 model
            if (not m_Items[i].m_pModel.Load(pModelStream, pModelStream.Size)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to load MD3 model - file name - ' +
                                               modelFileName                             +
                                               ' - class name - '                        +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // get mesh count
            frameCount := m_Items[i].m_pModel.GetMeshCount;

            // do create cache?
            if (doCreateCache) then
                // calculate step count
                totalStep := frameCount + 3.0
            else
                // get step count
                totalStep := 3.0;

            // calculate the value of a progress step
            progressStep := (totalItemStep / totalStep);

            // model is loaded, add one step to progress
            Progress := Progress + progressStep;

            // set model color
            m_Items[i].m_pModel.Color := m_pColor;

            // build skin file name
            skinFileName := TFileName(TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                                             m_PrefixKeyword,
                                                             UnicodeString(m_pInfo.m_pPrefixes[i])) +
                                      '.skin');

            // skin file exists?
            if (not m_pDir.FileExists(skinFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 skin stream does not exist - ' +
                                               skinFileName                        +
                                               ' - class name - '                  +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // get stream containing skin data
            pSkinStream := m_pDir.GetFile(skinFileName);

            // found it?
            if (not Assigned(pSkinStream)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to get MD3 skin stream - ' +
                                               modelName                          +
                                               ' - class name - '                 +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            // load md3 skin
            if (not LoadSkin(pSkinStream, m_Items[i])) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Failed to load MD3 skin - file name - ' +
                                               skinFileName                             +
                                               ' - class name - '                       +
                                               ClassName);
                {$endif}

                Exit(False);
            end;

            m_pLock.Lock;

            try
                textureLoaded := m_TextureLoaded;
            finally
                m_pLock.Unlock;
            end;

            // skin is loaded, add one step to progress
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
            m_Items[i].m_pModel.VertexFormat := vertexFormat;

            // add newly created item to dictionary (to facilitate search later)
            m_pItemDictionary.AddOrSetValue(UnicodeString(modelName), m_Items[i]);

            // model is configured and default mesh is created, add one step to progress
            Progress := Progress + progressStep;

            // do not create cache?
            if (not doCreateCache) then
            begin
                // set cache index, in case model needs to be cached later
                m_Items[i].m_CacheIndex := cacheIndex;
                Inc(cacheIndex, frameCount);
                continue;
            end;

            // something to cache?
            if (frameCount > 0) then
            begin
                // keep index from where item frames will be added in cache
                m_Items[i].m_CacheIndex := cacheIndex;

                // iterate through frames to cache
                for j := 0 to frameCount - 1 do
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
                    if (not m_Items[i].m_pModel.GetMesh(j, pMesh^, pTree, IsCanceled)) then
                    begin
                        {$ifdef DEBUG}
                            TQRLogHelper.LogToCompiler('MD3 model frame creation failed or was canceled - name - ' +
                                                       modelFileName                                               +
                                                       ' - index - '                                               +
                                                       IntToStr(Int64(j))                                          +
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
                        SetMesh(m_Items[i].m_CacheIndex + j, pMesh);
                    except
                        Dispose(pMesh);
                    end;

                    // do ignore collisions?
                    if (not(EQR_MO_No_Collision in ModelOptions)) then
                        // add tree to cache, note that from now cache will take care of the pointer
                        try
                            SetTree(m_Items[i].m_CacheIndex + j, pTree);
                        except
                            pTree.Free;
                        end;

                    // update next available cache index position
                    Inc(cacheIndex);

                    // a new frame was cached, add one step to progress
                    Progress := Progress + progressStep;
                end;
            end;
        end;

        LinkModel;

        // calculate the value of a progress step
        progressStep := (totalItemStep / 2.0);

        // model was linked, add one step to progress
        Progress := Progress + progressStep;

        // build animation file name
        animFileName := TFileName(TQRMD3Helper.BuildName(m_pInfo.m_AnimTemplate,
                                                         m_PrefixKeyword,
                                                         'animation') + '.cfg');

        // animation file exists?
        if (not m_pDir.FileExists(animFileName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MD3 animation stream does not exist - ' +
                                           animFileName                             +
                                           ' - class name - '                       +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // get stream containing animation data
        pAnimCfgStream := m_pDir.GetFile(animFileName);

        // found it?
        if (not Assigned(pAnimCfgStream)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to get MD3 animation stream - ' +
                                           modelName                               +
                                           ' - class name - '                      +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // open it
        if (not LoadAnimations(m_pInfo, pAnimCfgStream)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to load MD3 animation - file name - ' +
                                           animFileName                                  +
                                           ' - class name - '                            +
                                           ClassName);
            {$endif}

            Exit(False);
        end;

        // model is fully loaded
        Progress := 100.0;
        IsLoaded := True;
        Result   := True;
    finally
        TThread.Synchronize(nil, OnAfterLoadModel);
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRLoadMD3PackageJob
//--------------------------------------------------------------------------------------------------
constructor TQRLoadMD3PackageJob.Create(pGroup: TQRModelGroup;
                                const pPackage: TStream;
                           const prefixKeyword: UnicodeString;
                                  const pColor: TQRColor;
                                  modelOptions: TQRModelOptions;
                            framedModelOptions: TQRFramedModelOptions;
                                fOnLoadTexture: TQRLoadMeshTextureEvent;
                                          team: EQRMD3PackageTeam;
                                customTeamName: UnicodeString);
var
    pInfo: TQRMD3GroupInfo;
begin
    pInfo := TQRMD3GroupInfo.Create(True);

    try
        // create model info structure
        pInfo.ModelTemplate := '%s';
        pInfo.AnimTemplate  := '%s';

        // select team
        case (team) of
            EQR_PT_MD3_Default: pInfo.SkinTemplate := '%s_default';
            EQR_PT_MD3_Red:     pInfo.SkinTemplate := '%s_red';
            EQR_PT_MD3_Blue:    pInfo.SkinTemplate := '%s_blue';
            EQR_PT_MD3_Custom:  pInfo.SkinTemplate := '%s_' + customTeamName;
        else
            // unknown team, set to default
            pInfo.SkinTemplate := '%s_default';
        end;

        inherited Create(pGroup,
                         nil,
                         prefixKeyword,
                         pInfo,
                         pColor,
                         modelOptions,
                         framedModelOptions,
                         fOnLoadTexture);
    finally
        pInfo.Free;
    end;

    m_ExternalUnpackSucceeded := False;
    m_ExternalUnpackHandled   := False;
    m_fOnUnpackModel          := nil;

    // create local variables
    m_pDir  := TQRMemoryDir.Create(True);
    m_pIcon := TBitmap.Create;

    // copy values needed to load the model
    m_pPackage := pPackage;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLoadMD3PackageJob.Destroy;
begin
    m_pLock.Lock;

    try
        // clear memory
        m_pIcon.Free;
        m_pPackage.Free;
    finally
        m_pLock.Unlock;
    end;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3PackageJob.OnOpenInputZipStream(pSender: TObject; var pStream: TStream);
begin
    m_pLock.Lock;

    try
        m_pPackage.Position := 0;
        pStream             := TMemoryStream.Create;
        pStream.CopyFrom(m_pPackage, m_pPackage.Size);
        pStream.Position    := 0;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3PackageJob.OnCreateOutZipStream(pSender: TObject;
                                                var pStream: TStream;
                                                      pItem: TFullZipFileEntry);
begin
    // create a new stream to receive the next file content
    pStream := TMemorystream.Create;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3PackageJob.OnDoneOutZipStream(pSender: TObject;
                                              var pStream: TStream;
                                                    pItem: TFullZipFileEntry);
var
    pFileStream: TMemoryStream;
    fileName:    TFileName;
begin
    // is file stream available?
    if (not Assigned(pStream)) then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('MD3 - unpack - failed to extract stream from zip - ' +
                                       pItem.ArchiveFileName                                 +
                                       ' - class name - '                                    +
                                       ClassName);
        {$endif}

        Exit;
    end;

    pFileStream := nil;

    try
        // is a directory?
        if (pItem.IsDirectory) then
           Exit;

        // rewind zip stream
        pStream.Position := 0;

        // get next zipped file name (in lower case and without path)
        fileName := LowerCase(TFileName(TQRFileHelper.ExtractFileName(pItem.ArchiveFileName,
                                                                      CQR_Zip_Dir_Delimiter)));

        // file name should not be empty
        if (Length(fileName) = 0) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MD3 - unpack - found invalid file name - class name - ' +
                                           ClassName);
            {$endif}

            Exit;
        end;

        // file already exists in memory dir?
        if (m_pDir.FileExists(fileName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MD3 - unpack - found duplicate - file should be unique in package - ' +
                                           fileName                                                               +
                                           ' - class name - '                                                     +
                                           ClassName);
            {$endif}

            Exit;
        end;

        // copy zip stream content to memory stream
        pFileStream := TMemoryStream.Create;
        pFileStream.CopyFrom(pStream, pStream.Size);
        pFileStream.Position := 0;

        // add file to memory dir
        m_pDir.AddFile(fileName, pFileStream, False);

        pFileStream := nil;
    finally
        pFileStream.Free;
        pStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3PackageJob.Unpack: Boolean;
var
    pZipFile: TUnZipper;
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

        pZipFile := nil;

        try
            // create zipper instance
            pZipFile := TUnZipper.Create;

            pZipFile.OnOpenInputStream := OnOpenInputZipStream;
            pZipFile.OnCreateStream    := OnCreateOutZipStream;
            pZipFile.OnDoneStream      := OnDoneOutZipStream;

            // unzip files
            pZipFile.UnZipAllFiles;
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
function TQRLoadMD3PackageJob.GetIcon: TBitmap;
var
    fileName:    TFileName;
    extension:   UnicodeString;
    pIconStream: TStream;
    index:       NativeInt;
    iconExists:  Boolean;
begin
    m_pLock.Lock;

    try
        // no texture bitmap to load to?
        if (not Assigned(m_pIcon)) then
            Exit(nil);

        // icon was already loaded?
        if ((m_pIcon.Width > 0) and (m_pIcon.Height > 0)) then
            Exit(m_pIcon);

        index := 0;

        repeat
            // get next image extension to load
            extension := TextureExt[index];

            // build icon file name
            fileName := TFileName(TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                                         m_PrefixKeyword,
                                                         'icon') + extension);

            // check if icon file exists
            iconExists := m_pDir.FileExists(fileName);

            Inc(index);
        until (iconExists or (index >= TextureExtCount));

        // found a icon file to load?
        if (not iconExists) then
            Exit(nil);

        // get icon stream
        pIconStream := m_pDir.GetFile(fileName);

        // found it?
        if (not Assigned(pIconStream)) then
            Exit(nil);

        // load texture
        if (not TQRModelGroupHelper.LoadTexture(pIconStream, extension, m_pIcon)) then
            Exit(nil);

        Result := m_pIcon;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3PackageJob.GetShader: TStream;
begin
    m_pLock.Lock;

    try
        // is virtual dir assigned?
        if (not Assigned(m_pDir)) then
            Exit(nil);

        // no shader file?
        if (Length(m_ShaderFileName) = 0) then
            Exit(nil);

        // file exists?
        if (not m_pDir.FileExists(m_ShaderFileName)) then
            Exit(nil);

        // get file
        Result := m_pDir.GetFile(m_ShaderFileName);
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3PackageJob.GetSound(index: EQRMD3PackageSound): TStream;
var
    fileName: TFileName;
begin
    m_pLock.Lock;

    try
        // is virtual dir assigned?
        if (not Assigned(m_pDir)) then
            Exit(nil);

        // search for file name to use
        case (index) of
            EQR_PS_MD3_Death1:    fileName := 'death1.wav';
            EQR_PS_MD3_Death2:    fileName := 'death2.wav';
            EQR_PS_MD3_Death3:    fileName := 'death3.wav';
            EQR_PS_MD3_Drown:     fileName := 'drown.wav';
            EQR_PS_MD3_Fall1:     fileName := 'fall1.wav';
            EQR_PS_MD3_Falling1:  fileName := 'falling1.wav';
            EQR_PS_MD3_Gasp:      fileName := 'gasp.wav';
            EQR_PS_MD3_Jump1:     fileName := 'jump1.wav';
            EQR_PS_MD3_Pain25_1:  fileName := 'pain25_1.wav';
            EQR_PS_MD3_Pain50_1:  fileName := 'pain50_1.wav';
            EQR_PS_MD3_Pain75_1:  fileName := 'pain75_1.wav';
            EQR_PS_MD3_Pain100_1: fileName := 'pain100_1.wav';
            EQR_PS_MD3_Taunt:     fileName := 'taunt.wav';
        else
            fileName := '';
        end;

        // no sound file?
        if (Length(fileName) = 0) then
            Exit(nil);

        // file exists?
        if (not m_pDir.FileExists(fileName)) then
            Exit(nil);

        // get file
        Result := m_pDir.GetFile(fileName);
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3PackageJob.OnUnpackModelExternally;
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
                                                      m_ExternalUnpackHandled);

    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3PackageJob.Process: Boolean;
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
// TQRMD3Group
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Group.Create;
begin
    inherited Create;

    m_PrefixKeyword      := '%s';
    m_pJob               := nil;
    m_pPostponedGestures := TQRMD3ModelPostponedGestures.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Group.Destroy;
begin
    // delete model and his associated job, don't forget to unregister it from worker
    if (Assigned(m_pJob)) then
    begin
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    // clear postponed gestures
    m_pPostponedGestures.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.AnimateModel(elapsedTime: Double; pItem: TQRMD3ModelItem);
var
    pAnimItem:           TQRMD3AnimationItem;
    frameIndex:          NativeUInt;
    interpolationFactor: Double;
    endNotified:         Boolean;
begin
    // no model item to animate?
    if (not Assigned(pItem)) then
        Exit;

    // no selected gesture?
    if (pItem.m_Gesture >= EQR_AG_MD3_Max_Animations) then
        Exit;

    // model contains no animation?
    if (pItem.m_pAnimations.Count = 0) then
    begin
        pItem.m_Gesture := EQR_AG_MD3_Max_Animations;
        Exit;
    end;

    // animation info exists in dictionary?
    if (not pItem.m_pAnimations.ContainsKey(pItem.m_Gesture)) then
    begin
        pItem.m_Gesture := EQR_AG_MD3_Max_Animations;
        Exit;
    end;

    // extract animation info item
    pAnimItem := pItem.m_pAnimations[pItem.m_Gesture];

    // found it?
    if (not Assigned(pAnimItem)) then
    begin
        pItem.m_Gesture := EQR_AG_MD3_Max_Animations;
        Exit;
    end;

    frameIndex          := pItem.m_pAnimation.FrameIndex;
    interpolationFactor := pItem.m_pAnimation.InterpolationFactor;

    // clear previous animation
    pItem.m_pAnimation.Free;

    endNotified := False;

    // animate model
    pItem.m_pAnimation := GetAnimation(pItem.m_pModel,
                                       NativeInt(pItem.m_Gesture),
                                       elapsedTime,
                                       pAnimItem.m_FramesPerSecond,
                                       pAnimItem.m_StartFrame,
                                       pAnimItem.m_EndFrame,
                                       pAnimItem.m_Loop,
                                       frameIndex,
                                       interpolationFactor,
                                       endNotified);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.GetDynamicMesh(const pItem: TQRMD3ModelItem;
                                           index: NativeUInt;
                                        out mesh: TQRMesh);
var
    frameCount: NativeUInt;
begin
    // no sub-model item?
    if (not Assigned(pItem)) then
        Exit;

    // no sub-model to draw?
    if (not Assigned(pItem.m_pModel)) then
        Exit;

    // get frame count
    frameCount := pItem.m_pModel.GetMeshCount;

    // model contains meshes?
    if (frameCount = 0) then
        Exit;

    // is mesh index out of bounds?
    if (index >= frameCount) then
        Exit;

    // get mesh
    if (not pItem.m_pModel.GetMesh(index,
                                   mesh,
                                   TQRAABBTree(nil),
                                   TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('MD3 model frame creation failed - index - ' +
                                       IntToStr(index)                              +
                                       ' - class name - '                           +
                                       ClassName);
        {$endif}
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.GetDynamicMeshUseCache(const pItem: TQRMD3ModelItem;
                                                   index: NativeUInt;
                                               out pMesh: PQRMesh;
                                               out pTree: TQRAABBTree);
var
    frameCount:    NativeUInt;
    useCollisions: Boolean;
begin
    pMesh := nil;
    pTree := nil;

    // no sub-model item?
    if (not Assigned(pItem)) then
        Exit;

    // no sub-model to draw?
    if (not Assigned(pItem.m_pModel)) then
        Exit;

    // get frame count
    frameCount := pItem.m_pModel.GetMeshCount;

    // model contains meshes?
    if (frameCount = 0) then
        Exit;

    // is mesh index out of bounds?
    if (index >= frameCount) then
        Exit;

    useCollisions := not (EQR_MO_No_Collision in m_pJob.ModelOptions);

    // get mesh from cache
    pMesh := m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex];

    // do create collision buffers?
    if (useCollisions) then
        // get AABB tree from cache
        pTree := m_pJob.AABBTree[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex];

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
    if (not pItem.m_pModel.GetMesh(pItem.m_pAnimation.FrameIndex,
                                   pMesh^,
                                   pTree,
                                   TQRIsCanceledEvent(nil)))
    then
    begin
        {$ifdef DEBUG}
            TQRLogHelper.LogToCompiler('MD3 model frame creation failed - index - ' +
                                       IntToStr(pItem.m_pAnimation.FrameIndex)      +
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
        m_pJob.Mesh[pItem.m_CacheIndex + index] := pMesh;
    except
        Dispose(pMesh);
    end;

    // do create collision buffers?
    if (useCollisions) then
        // add tree to cache, note that from now cache will take care of the pointer
        try
            m_pJob.AABBTree[pItem.m_CacheIndex + index] := pTree;
        except
            pTree.Free;
        end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.DrawDynamicModel(const pItem: TQRMD3ModelItem; const matrix: TQRMatrix4x4);
var
    pMesh, pNextMesh: PQRMesh;
    interpolatedMesh: TQRMesh;
    pTree, pNextTree: TQRAABBTree;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
        Exit;

    // no sub-model item?
    if (not Assigned(pItem)) then
        Exit;

    // no sub-model to draw?
    if (not Assigned(pItem.m_pModel)) then
        Exit;

    // can use dynamic cache?
    if (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions) then
    begin
        // do interpolate?
        if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
        begin
            // get mesh to draw
            pItem.m_pModel.GetMesh(pItem.m_pAnimation.FrameIndex,
                                   pItem.m_pAnimation.InterpolationFrameIndex,
                                   pItem.m_pAnimation.InterpolationFactor,
                                   interpolatedMesh,
                                   TQRIsCanceledEvent(nil));

            // draw mesh
            OnDrawItem(Self,
                       pItem.m_pModel,
                       pItem.m_Textures,
                       matrix,
                       pItem.m_pAnimation.FrameIndex,
                       pItem.m_pAnimation.InterpolationFrameIndex,
                       pItem.m_pAnimation.InterpolationFactor,
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
                GetDynamicMesh(pItem, pItem.m_pAnimation.FrameIndex,              pMesh^);
                GetDynamicMesh(pItem, pItem.m_pAnimation.InterpolationFrameIndex, pNextMesh^);

                // draw mesh
                OnDrawItem(Self,
                           pItem.m_pModel,
                           pItem.m_Textures,
                           matrix,
                           pItem.m_pAnimation.FrameIndex,
                           pItem.m_pAnimation.InterpolationFrameIndex,
                           pItem.m_pAnimation.InterpolationFactor,
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
    GetDynamicMeshUseCache(pItem, pItem.m_pAnimation.FrameIndex,              pMesh,     pTree);
    GetDynamicMeshUseCache(pItem, pItem.m_pAnimation.InterpolationFrameIndex, pNextMesh, pNextTree);

    // do interpolate?
    if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
    begin
        // interpolate meshes
        TQRModelHelper.Interpolate(pItem.m_pAnimation.InterpolationFactor,
                                   pMesh^,
                                   pNextMesh^,
                                   interpolatedMesh);

        // draw mesh
        OnDrawItem(Self,
                   pItem.m_pModel,
                   pItem.m_Textures,
                   matrix,
                   pItem.m_pAnimation.FrameIndex,
                   pItem.m_pAnimation.InterpolationFrameIndex,
                   pItem.m_pAnimation.InterpolationFactor,
                   @interpolatedMesh,
                   nil,
                   pTree,
                   pNextTree);
    end
    else
        // draw mesh
        OnDrawItem(Self,
                   pItem.m_pModel,
                   pItem.m_Textures,
                   matrix,
                   pItem.m_pAnimation.FrameIndex,
                   pItem.m_pAnimation.InterpolationFrameIndex,
                   pItem.m_pAnimation.InterpolationFactor,
                   pMesh,
                   pNextMesh,
                   pTree,
                   pNextTree);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.DrawCachedModel(const pItem: TQRMD3ModelItem; const matrix: TQRMatrix4x4);
var
    interpolatedMesh: TQRMesh;
begin
    // nothing to draw?
    if (not Assigned(OnDrawItem)) then
        Exit;

    // no sub-model item?
    if (not Assigned(pItem)) then
        Exit;

    // no sub-model to draw?
    if (not Assigned(pItem.m_pModel)) then
        Exit;

    // collision buffers were created?
    if (EQR_MO_No_Collision in m_pJob.ModelOptions) then
    begin
        // do interpolate?
        if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
        begin
            // interpolate meshes
            TQRModelHelper.Interpolate(pItem.m_pAnimation.InterpolationFactor,
                                       m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex]^,
                                       m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex]^,
                                       interpolatedMesh);

            // draw mesh
            OnDrawItem(Self,
                       pItem.m_pModel,
                       pItem.m_Textures,
                       matrix,
                       pItem.m_pAnimation.FrameIndex,
                       pItem.m_pAnimation.InterpolationFrameIndex,
                       pItem.m_pAnimation.InterpolationFactor,
                       @interpolatedMesh,
                       nil,
                       nil,
                       nil);
        end
        else
            // draw mesh
            OnDrawItem(Self,
                       pItem.m_pModel,
                       pItem.m_Textures,
                       matrix,
                       pItem.m_pAnimation.FrameIndex,
                       pItem.m_pAnimation.InterpolationFrameIndex,
                       pItem.m_pAnimation.InterpolationFactor,
                       m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex],
                       m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex],
                       nil,
                       nil);
    end
    else
    // do interpolate?
    if (EQR_FO_Interpolate in m_pJob.FramedModelOptions) then
    begin
        // interpolate meshes
        TQRModelHelper.Interpolate(pItem.m_pAnimation.InterpolationFactor,
                                   m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex]^,
                                   m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex]^,
                                   interpolatedMesh);

        // draw mesh
        OnDrawItem(Self,
                   pItem.m_pModel,
                   pItem.m_Textures,
                   matrix,
                   pItem.m_pAnimation.FrameIndex,
                   pItem.m_pAnimation.InterpolationFrameIndex,
                   pItem.m_pAnimation.InterpolationFactor,
                   @interpolatedMesh,
                   nil,
                   m_pJob.AABBTree[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex],
                   m_pJob.AABBTree[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex])
    end
    else
        // draw mesh
        OnDrawItem(Self,
                   pItem.m_pModel,
                   pItem.m_Textures,
                   matrix,
                   pItem.m_pAnimation.FrameIndex,
                   pItem.m_pAnimation.InterpolationFrameIndex,
                   pItem.m_pAnimation.InterpolationFactor,
                   m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex],
                   m_pJob.Mesh[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex],
                   m_pJob.AABBTree[pItem.m_CacheIndex + pItem.m_pAnimation.FrameIndex],
                   m_pJob.AABBTree[pItem.m_CacheIndex + pItem.m_pAnimation.InterpolationFrameIndex]);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.DrawMesh(const pItem: TQRMD3ModelItem; const matrix: TQRMatrix4x4);
var
    pParser:                          TQRMD3Parser;
    interpolationFactor:              Double;
    pLink:                            TQRMD3ModelLink;
    tagCount, tagIndex, nextTagIndex: NativeUInt;
    pTag, pNextTag:                   PQRMD3Tag;
    pos:                              TQRVector3D;
    quat, nextQuat, interpolatedQuat: TQRQuaternion;
    interpolatedMatrix:               TQRMatrix4x4;
begin
    // no sub-model item?
    if (not Assigned(pItem)) then
        Exit;

    // no sub-model to draw?
    if (not Assigned(pItem.m_pModel)) then
        Exit;

    // can draw model from a previously built cache, do generate frames dynamically, or let user
    // take care of frames creation?
    if (EQR_MO_Create_Cache in m_pJob.ModelOptions) then
        DrawCachedModel(pItem, matrix)
    else
    if ((EQR_MO_Dynamic_Frames          in m_pJob.ModelOptions) or
        (EQR_MO_Dynamic_Frames_No_Cache in m_pJob.ModelOptions))
    then
        DrawDynamicModel(pItem, matrix)
    else
    if (Assigned(OnCustomDrawItem))
    then
        // let user take care of drawing model
        OnCustomDrawItem(Self,
                         pItem.m_pModel,
                         pItem.m_Textures,
                         matrix,
                         pItem.m_pAnimation.FrameIndex,
                         pItem.m_pAnimation.InterpolationFrameIndex,
                         pItem.m_pAnimation.InterpolationFactor);

    // get model parser
    pParser := pItem.m_pModel.Parser;

    // found it?
    if (not Assigned(pParser)) then
        Exit;

    // get interpolation factor
    interpolationFactor := pItem.m_pAnimation.InterpolationFactor;

    // no links?
    if (Length(pItem.m_LinksTo) = 0) then
        Exit;

    // iterate through model tags
    for pLink in pItem.m_LinksTo do
    begin
        // get tag count (tags contain some important link info as e.g. position and rotation matrix)
        tagCount := pParser.Header.m_TagCount;

        // get current frame tag
        tagIndex := (pItem.m_pAnimation.FrameIndex * tagCount) + pLink.m_TagIndex;
        pTag     := pParser.Tags[tagIndex];

        // get frame tag to interpolate with
        nextTagIndex := (pItem.m_pAnimation.InterpolationFrameIndex * tagCount) + pLink.m_TagIndex;
        pNextTag     := pParser.Tags[nextTagIndex];

        // get interpolated linked sub-model position
        pos := TQRVector3D.Create(pTag.m_Position[0] + (interpolationFactor * (pNextTag.m_Position[0] - pTag.m_Position[0])),
                                  pTag.m_Position[1] + (interpolationFactor * (pNextTag.m_Position[1] - pTag.m_Position[1])),
                                  pTag.m_Position[2] + (interpolationFactor * (pNextTag.m_Position[2] - pTag.m_Position[2])));

        // get linked sub-model frame matrix, convert it to a quaternion
        quat := TQRQuaternion.Create(TQRMatrix4x4.Create(pTag.m_Rotation[0][0], pTag.m_Rotation[1][0], pTag.m_Rotation[2][0], 0.0,
                                                         pTag.m_Rotation[0][1], pTag.m_Rotation[1][1], pTag.m_Rotation[2][1], 0.0,
                                                         pTag.m_Rotation[0][2], pTag.m_Rotation[1][2], pTag.m_Rotation[2][2], 0.0,
                                                         0.0,                   0.0,                   0.0,                   1.0));

        // get linked sub-model next frame matrix, convert it to a quaternion
        nextQuat :=
                TQRQuaternion.Create(TQRMatrix4x4.Create(pNextTag.m_Rotation[0][0], pNextTag.m_Rotation[1][0], pNextTag.m_Rotation[2][0], 0.0,
                                                         pNextTag.m_Rotation[0][1], pNextTag.m_Rotation[1][1], pNextTag.m_Rotation[2][1], 0.0,
                                                         pNextTag.m_Rotation[0][2], pNextTag.m_Rotation[1][2], pNextTag.m_Rotation[2][2], 0.0,
                                                         0.0,                       0.0,                       0.0,                       1.0));

        // interpolate quaternions using spherical linear interpolation
        interpolatedQuat := quat.Slerp(nextQuat, interpolationFactor);

        // get resulting interpolated matrix
        interpolatedMatrix := interpolatedQuat.GetMatrix;

        // set interpolated position inside resulting matrix directly
        interpolatedMatrix.Table[3, 0] := pos.X;
        interpolatedMatrix.Table[3, 1] := pos.Y;
        interpolatedMatrix.Table[3, 2] := pos.Z;

        // draw next linked sub-model
        DrawMesh(pLink.m_pItem, interpolatedMatrix.Multiply(matrix));
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetMemoryDir: TQRMemoryDir;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(nil);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(nil);

    // is job a memory dir job?
    if (m_pJob is TQRLoadMD3MemoryDirJob) then
        // get and return memory dir
        Exit(TQRLoadMD3MemoryDirJob(m_pJob).MemoryDir);

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetIcon: TBitmap;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(nil);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(nil);

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
        // get and return icon
        Exit(TQRLoadMD3PackageJob(m_pJob).Icon);

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetShader: TStream;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(nil);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(nil);

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
        // get and return shader file
        Exit(TQRLoadMD3PackageJob(m_pJob).Shader);

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetSound(index: EQRMD3PackageSound): TStream;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit(nil);

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        Exit(nil);

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
        // get and return sound
        Exit(TQRLoadMD3PackageJob(m_pJob).Sound[index]);

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.Clear;
begin
    // delete model and his associated job, don't forget to unregister it from worker
    if (Assigned(m_pJob)) then
    begin
        TQRModelWorker.GetInstance.CancelJob(m_pJob);
        m_pJob := nil;
    end;

    m_pPostponedGestures.Clear;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.IsEmpty: Boolean;
begin
    Result := (not Assigned(m_pJob));
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.Load(const dir: UnicodeString;
                        const pInfo: TQRMD3GroupInfo;
                       const pColor: TQRColor;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions): Boolean;
begin
    // clear previous group instance
    Clear;

    // prepare model job to load from file
    m_pJob := TQRLoadMD3FileJob.Create(Self,
                                       dir,
                                       m_PrefixKeyword,
                                       pInfo,
                                       pColor,
                                       modelOptions,
                                       framedModelOptions,
                                       OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.Load(const pDir: TQRMemoryDir;
                         const pInfo: TQRMD3GroupInfo;
                        const pColor: TQRColor;
                              rhToLh: Boolean;
                        modelOptions: TQRModelOptions;
                  framedModelOptions: TQRFramedModelOptions): Boolean;
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

    // prepare model job to load from memory dir
    m_pJob := TQRLoadMD3MemoryDirJob.Create(Self,
                                            pDir,
                                            m_PrefixKeyword,
                                            pInfo,
                                            pColor,
                                            modelOptions,
                                            framedModelOptions,
                                            OnLoadMeshTexture);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.Load(const fileName: TFileName;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam;
                          customTeamName: UnicodeString): Boolean;
var
    pPackage: TFileStream;
begin
    // file exists?
    if (not FileExists(fileName)) then
        Exit(False);

    // open a stream from file
    pPackage := TFileStream.Create(fileName, fmOpenRead);

    // load package
    Result := Load(pPackage, pColor, rhToLh, modelOptions, framedModelOptions, team, customTeamName);
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.Load(const pPackage: TStream;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam;
                          customTeamName: UnicodeString): Boolean;
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
    m_pJob := TQRLoadMD3PackageJob.Create(Self,
                                          pPackage,
                                          m_PrefixKeyword,
                                          pColor,
                                          modelOptions,
                                          framedModelOptions,
                                          OnLoadMeshTexture,
                                          team,
                                          customTeamName);

    // execute the job
    TQRModelWorker.GetInstance.StartJob(m_pJob);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.SetAnimation(const name: UnicodeString;
                                     gesture: EQRMD3AnimationGesture): Boolean;
var
    pItem:  TQRMD3ModelItem;
    doLoop: Boolean;
begin
    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        // model is not ready, postpone gesture
        m_pPostponedGestures.AddOrSetValue(name, gesture);
        Exit(True);
    end;

    // animation dictionary contains sub-model for which animation should be changed?
    if (not m_pJob.m_pItemDictionary.ContainsKey(name)) then
        Exit(False);

    // get sub-model item
    pItem := m_pJob.m_pItemDictionary[name];

    // found it?
    if (not Assigned(pItem)) then
        Exit(False);

    // is gesture supported by sub-model?
    if (not pItem.m_pAnimations.ContainsKey(gesture)) then
        Exit(False);

    doLoop := pItem.m_pAnimations[gesture].Loop;

    // update loop value
    pItem.m_pAnimation.Loop := doLoop;

    // do loop animation?
    if (not doLoop) then
    begin
        pItem.m_pAnimation.FrameIndex              := pItem.m_pAnimations[gesture].m_StartFrame;
        pItem.m_pAnimation.InterpolationFrameIndex := pItem.m_pAnimations[gesture].m_StartFrame;
        pItem.m_pAnimation.InterpolationFactor     := 0.0;
    end;

    // change sub-model gesture
    pItem.m_Gesture := gesture;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.QueryJobStatus: TQRModelJobStatus;
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
procedure TQRMD3Group.Draw(const elapsedTime: Double);
var
    pItem:                TQRMD3ModelItem;
    modelMatrix:          TQRMatrix4x4;
    postponedGestureItem: TPair<UnicodeString, EQRMD3AnimationGesture>;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
        Exit;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
        // unfortunately, it's not possible to apply the EQR_FO_Start_Anim_When_Gesture_Is_Ready and
        // EQR_FO_Show_Default_Frame to a MD3 model, because it's composed of many sub-models, that
        // are loaded one after the other. Since these options may only be applied after the last
        // sub-model is loaded, there is nothing to do here
        Exit;

    // some gestures were postponed?
    if (m_pPostponedGestures.Count > 0) then
    begin
        // apply all postponed gestures
        for postponedGestureItem in m_pPostponedGestures do
            if (not SetAnimation(postponedGestureItem.Key, postponedGestureItem.Value)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('Could not apply all postponed gestures - '                +
                                               postponedGestureItem.Key                                   +
                                               ' - gesture - '                                            +
                                               TQRMD3AnimCfgFile.GestureToStr(postponedGestureItem.Value) +
                                               ' - class name - '                                         +
                                               ClassName);
                {$endif}
            end;

        // now gestures are applied, clear the dictionary
        m_pPostponedGestures.Clear;
    end;

    // no sub-models to draw?
    if (Length(m_pJob.m_Items) = 0) then
        Exit;

    // animate each sub-model
    for pItem in m_pJob.m_Items do
        AnimateModel(elapsedTime, pItem);

    // get model matrix
    modelMatrix := GetMatrix;

    // draw all sub-models, recursively
    DrawMesh(m_pJob.m_Items[0], modelMatrix);
end;
//--------------------------------------------------------------------------------------------------

end.
