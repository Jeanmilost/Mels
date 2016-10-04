{**************************************************************************************************
 * ==> UTQRMD3ModelGroup -------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module contains the classes used to load and link all MD3 files together.   *
 *               The MD3 package files (.pk3) are also supported                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRMD3ModelGroup;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     System.Generics.Collections,
     System.Zip,
     UTQRCommon,
     UTQRLogging,
     UTQRHelpers,
     UTQRFiles,
     UTQRThreading,
     UTQRGraphics,
     UTQRGeometry,
     UTQR3D,
     UTQRCollision,
     UTQRModel,
     UTQRMD3,
     UTQRModelGroup,
     Vcl.Graphics;

type
    {**
    * MD3 model helper
    *}
    TQRMD3Helper = class
        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Builds name based on a template
            *@param templateName - template name to build from
            *@param prefixKeyword - prefix keyword to find and replace in template name
            *@param prefix - prefix to use
            *@return built name
            *}
            class function BuildName(const templateName,
                                          prefixKeyword,
                                                 prefix: UnicodeString): UnicodeString; static;

            {**
            * Builds tag name
            *@param itemName - item name
            *@return tag name, empty string if failed or on error
            *}
            class function BuildTagName(const itemName: UnicodeString): UnicodeString; static;
    end;

    {**
    * Standard MD3 animation set, as commonly defined in the Quake engine
    *@note These gestures are given for convenience, you are free to define your own gestures
    *}
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

    {**
    * MD3 model gender
    *}
    EQRMD3Gender =
    (
        EQR_GN_MD3_Unknown = 0,
        EQR_GN_MD3_Male,
        EQR_GN_MD3_Female
    );

    {**
    * Foot step mode
    *}
    EQRMD3FootStep =
    (
        EQR_FS_MD3_Unknown = 0,
        EQR_FS_MD3_Boot
    );

    {**
    * Structure containing headoffset instruction, don't know what exactly means
    *}
    TQRMD3AnimCfgFileHeadOffset = record
        m_UnknownOffset1: NativeInt;
        m_UnknownOffset2: NativeInt;
        m_UnknownOffset3: NativeInt;
    end;

    PQRMD3AnimCfgFileHeadOffset = ^TQRMD3AnimCfgFileHeadOffset;

    {**
    * Structure containing footsteps instruction, don't know what exactly means
    *}
    TQRMD3AnimCfgFileFootSteps = record
        m_Mode: EQRMD3FootStep;
    end;

    PQRMD3AnimCfgFileFootSteps = ^TQRMD3AnimCfgFileFootSteps;

    {**
    * MD3 animation configuration file parser
    *}
    TQRMD3AnimCfgFile = class(TQRFramedModelAnimCfgFile)
        protected
            m_Gender:         EQRMD3Gender;
            m_HeadOffset:     TQRMD3AnimCfgFileHeadOffset;
            m_FootSteps:      TQRMD3AnimCfgFileFootSteps;
            m_StartLine:      NativeUInt;
            m_CurLine:        NativeUInt;
            m_ReadGender:     Boolean;
            m_ReadHeadOffset: Boolean;
            m_ReadFootSteps:  Boolean;

            {**
            * Called when script line should be parsed
            *@param line - line to parse
            *@param linbeNb - line number
            *@return ture on success, otherwise false
            *}
            function OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean; override;

            {**
            * Parses a word found in script line
            *@param word - word to parse
            *@param lineNb - current parsing line number
            *@return true on success, otherwise false
            *}
            function ParseWord(const word: UnicodeString; lineNb: NativeUInt): Boolean; override;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Clears animation configuration
            *}
            procedure Clear(); override;

            {**
            * Gets model gender
            *@return model gender
            *}
            function GetGender(): EQRMD3Gender; virtual;

            {**
            * Gets header offset
            *@return header offset
            *}
            function GetHeadOffset(): PQRMD3AnimCfgFileHeadOffset; virtual;

            {**
            * Gets foot steps
            *@return foot steps
            *}
            function GetFootSteps(): PQRMD3AnimCfgFileFootSteps; virtual;

            {**
            * Converts animation gesture to string
            *@param gesture - animation gesture to convert
            *@return converted animation gesture as string
            *}
            class function GestureToStr(gesture: EQRMD3AnimationGesture): UnicodeString; static;

            { Properties }
            property Gender:     EQRMD3Gender                read GetGender;
            property HeadOffset: PQRMD3AnimCfgFileHeadOffset read GetHeadOffset;
            property FootSteps:  PQRMD3AnimCfgFileFootSteps  read GetFootSteps;
    end;

    TQRMD3PathTable = TDictionary<UnicodeString, UnicodeString>;
    TQRMD3LinkKeys  = TStringList;

    {**
    * MD3 skin file parser
    *}
    TQRMD3Skin = class(TQRScript)
        protected
            m_PathTable: TQRMD3PathTable;
            m_LinkKeys:  TQRMD3LinkKeys;

            {**
            * Called when script line should be parsed
            *@param line - line to parse
            *@param linbeNb - line number
            *@return ture on success, otherwise false
            *}
            function OnParseLine(const line: UnicodeString; lineNb: NativeUInt): Boolean; override;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Clears skin configuration
            *}
            procedure Clear(); override;

            {**
            * Gets path from table matching with name
            *@param name - path name to get
            *@return path, empty string if not found or on error
            *}
            function GetPath(const name: UnicodeString): UnicodeString; virtual;

            { Properties }
            property Path[const name: UnicodeString]: UnicodeString read GetPath;
    end;

    {**
    * Animation item, contains the information that defines a given animation
    *}
    TQRMD3AnimationItem = class
        protected
            m_StartFrame:      NativeUInt;
            m_EndFrame:        NativeUInt;
            m_FramesPerSecond: NativeUInt;
            m_Loop:            Boolean;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; virtual;

            {**
            * Constructor
            *@param startFrame - animation start frame index
            *@param endFrame - animation end frame index
            *@param fps - frame per seconds
            *@param loop - if true, animation should loop at end
            *}
            constructor Create(startFrame, endFrame, fps: NativeUInt;
                                                    loop: Boolean); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            { Properties }
            property StartFrame:      NativeUInt read m_StartFrame      write m_StartFrame;
            property EndFrame:        NativeUInt read m_EndFrame        write m_EndFrame;
            property FramesPerSecond: NativeUInt read m_FramesPerSecond write m_FramesPerSecond;
            property Loop:            Boolean    read m_Loop            write m_Loop;

    end;

    TQRMD3AnimationDictionary = TDictionary<EQRMD3AnimationGesture, TQRMD3AnimationItem>;

    {**
    * Link, it's a relationship between sub-models to animate
    *}
    TQRMD3ModelLink = class
        protected
            m_TagIndex: NativeUInt;
            m_pItem:    Pointer;

        public
            {**
            * Constructor
            *}
            constructor Create; overload; virtual;

            {**
            * Constructor
            *@param tagIndex - tag index in MD3 model from which link was built
            *@param pItem - item to link to
            *}
            constructor Create(tagIndex: NativeUInt; const pItem: Pointer); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy; override;

            { Properties }
            property TagIndex: NativeUInt read m_TagIndex write m_TagIndex;
            property Item:     Pointer    read m_pItem    write m_pItem;
    end;

    TQRMD3ModelLinks = array of TQRMD3ModelLink;

    {**
    * Group sub-model item, contains all info about a particular model belonging to group
    *}
    TQRMD3ModelItem = class
        protected
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

            {**
            * Adds a target model in links
            *@param links - links in which target should be added
            *@param pTarget - target to add to links
            *@param tagIndex - model tag index from which link was generated
            *}
            procedure AddLink(var links: TQRMD3ModelLinks;
                          const pTarget: TQRMD3ModelItem;
                               tagIndex: NativeUInt); virtual;

            {**
            * Gets model animation
            *@param gesture - animation gesture
            *@return animation item
            *}
            function GetAnimation(gesture: EQRMD3AnimationGesture): TQRMD3AnimationItem; virtual;

            {**
            * Sets model animation
            *@param gesture - animationm gesture
            *@param pAnimation - animation item to set
            *}
            procedure SetAnimation(gesture: EQRMD3AnimationGesture;
                          const pAnimation: TQRMD3AnimationItem); virtual;

            {**
            * Gets model texture at index
            *@param index - texture index
            *}
            function GetTexture(index: NativeInt): TQRTexture; virtual;

            {**
            * Gets link from a parent model
            *@param index - link index
            *@param pTarget - linked parent model
            *}
            function GetLinkFrom(index: NativeInt): TQRMD3ModelLink; virtual;

            {**
            * Gets link to a child model
            *@param index - link index
            *@param pTarget - linked child model
            *}
            function GetLinkTo(index: NativeInt): TQRMD3ModelLink; virtual;

            {**
            * Gets model texture count
            *@return model texture count
            *}
            function GetTextureCount: NativeInt; virtual;

            {**
            * Gets links from count
            *@return links from count
            *}
            function GetLinksFromCount: NativeInt; virtual;

            {**
            * Gets links to count
            *@return links to count
            *}
            function GetLinksToCount: NativeInt; virtual;

            {**
            * Gets model animation count
            *@return model animation count
            *}
            function GetAnimationCount: NativeInt; virtual;

        public
            { Construction/Destruction }
            constructor Create;  virtual;
            destructor  Destroy; override;

            {**
            * Adds a model texture
            *@param pTexture - texture to add
            *}
            procedure AddTexture(const pTexture: TQRTexture); virtual;

            {**
            * Adds link from a parent model
            *@param tagIndex - model tag index from which link was generated
            *@param pTarget - parent target model to link from
            *}
            procedure AddLinkFrom(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem); virtual;

            {**
            * Adds link to a child model
            *@param tagIndex - model tag index from which link was generated
            *@param pTarget - child target model to link to
            *}
            procedure AddLinkTo(tagIndex: NativeUInt; const pTarget: TQRMD3ModelItem); virtual;

            { Properties }
            property Name:                                        UnicodeString           read m_Name       write m_Name;
            property Model:                                       TQRMD3Model             read m_pModel     write m_pModel;
            property Skin:                                        TQRMD3Skin              read m_pSkin      write m_pSkin;
            property Animations[gesture: EQRMD3AnimationGesture]: TQRMD3AnimationItem     read GetAnimation write SetAnimation;
            property Animation:                                   TQRFramedModelAnimation read m_pAnimation write m_pAnimation;
            property Gesture:                                     EQRMD3AnimationGesture  read m_Gesture    write m_Gesture;
            property RhToLh:                                      Boolean                 read m_RhToLh     write m_RhToLh;
            property Textures  [index: NativeInt]:                TQRTexture              read GetTexture;
            property LinksFrom [index: NativeInt]:                TQRMD3ModelLink         read GetLinkFrom;
            property LinksTo   [index: NativeInt]:                TQRMD3ModelLink         read GetLinkTo;
            property TextureCount:                                NativeInt               read GetTextureCount;
            property LinksFromCount:                              NativeInt               read GetLinksFromCount;
            property LinksToCount:                                NativeInt               read GetLinksToCount;
            property AnimationCount:                              NativeInt               read GetAnimationCount;
            property CacheIndex:                                  NativeUInt              read m_CacheIndex;
    end;

    TQRMD3ModelItems = array of TQRMD3ModelItem;

    {**
    * Group info, contains all informations to load group, as e.g. file templates, prefixes, ...
    *}
    TQRMD3GroupInfo = class
        protected
            m_ModelTemplate: UnicodeString;
            m_SkinTemplate:  UnicodeString;
            m_AnimTemplate:  UnicodeString;
            m_pPrefixes:     TStringList;

        public
            {**
            * Constructor
            *@param createDefaultPrefixes - if true, default prefixes will be created
            *@note First declared prefix always matchs with the root model
            *@note Be careful, the prefix order will influence on the manner the objects will be
            *      linked. For example, a head/upper/lower order will link the head to upper model,
            *      then the upper to lower model, whereas a lower/upper/head order will link the
            *      lower to upper model, then the upper to head model
            *}
            constructor Create(createDefaultPrefixes: Boolean = true); overload; virtual;

            {**
            * Constructor
            *@param pOther - other group info to copy from
            *}
            constructor Create(const pOther: TQRMD3GroupInfo); overload; virtual;

            {**
            * Destructor
            *}
            destructor Destroy(); override;

            {**
            * Gets prefix at index
            *@param index . prefix index to get
            *@return prefix
            *}
            function GetPrefix(index: NativeInt): UnicodeString; virtual;

            {**
            * Gets prefix count
            *@return prefix count
            *}
            function GetPrefixCount(): NativeInt; virtual;

            {**
            * Adds prefix to list
            *@param prefix - prefix to add
            *}
            procedure AddPrefix(const prefix: UnicodeString); virtual;

            { Properties }
            property ModelTemplate:              UnicodeString read m_ModelTemplate write m_ModelTemplate;
            property SkinTemplate:               UnicodeString read m_SkinTemplate  write m_SkinTemplate;
            property AnimTemplate:               UnicodeString read m_AnimTemplate  write m_AnimTemplate;
            property Prefixes[index: NativeInt]: UnicodeString read GetPrefix;
            property PrefixCount:                NativeInt     read GetPrefixCount;
    end;

    TQRMD3ItemDictionary = TDictionary<UnicodeString, ^TQRMD3ModelItem>;

    {**
    * Generic MD3 job
    *}
    TQRMD3Job = class(TQRModelJob)
        protected
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

            {**
            * Gets model item at index
            *@return model item
            *}
            function GetItem(index: NativeInt): TQRMD3ModelItem; overload; virtual;

            {**
            * Gets model item from name
            *@return model item
            *}
            function GetItem(const name: UnicodeString): TQRMD3ModelItem; overload; virtual;

            {**
            * Gets framed model options
            *@return model options
            *}
            function GetFramedModelOptions(): TQRFramedModelOptions; virtual;

            {**
            * Sets framed model options
            *@param options - model options
            *}
            procedure SetFramedModelOptions(options: TQRFramedModelOptions); virtual;

            {**
            * Links all models loaded or added in group together
            *}
            procedure LinkModel(); virtual;

            {**
            * Links skin elements with model
            *@param pSkin - skin containing elements to load
            *@param dir - model directory
            *@param pItem - item containing sub-model to link with skin
            *@return true on success, otherwise false
            *}
            function LinkSkin(const pSkin: TQRMD3Skin;
                                const dir: UnicodeString;
                              const pItem: TQRMD3ModelItem): Boolean; virtual;

            {**
            * Links animations with model
            *@param pInfo - group info
            *@param pAnimations - configuration file containing animations
            *@return true on success, otherwise false
            *}
            function LinkAnimations(const pInfo: TQRMD3GroupInfo;
                              const pAnimations: TQRMD3AnimCfgFile): Boolean; virtual;

            {**
            * Called when model texture should be loaded
            *@note This function is executed on the calling thread side
            *}
            procedure OnLoadTexture(); virtual;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param pInfo - model info
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;

            {**
            * Cancels the job
            *}
            procedure Cancel(); override;

            {**
            * Checks if job was canceled
            *@return true if job was canceled, otherwise false
            *}
            function IsCanceled(): Boolean; virtual;

            { Properties }
            property ItemAtIndex[index:      NativeInt]:      TQRMD3ModelItem       read GetItem;
            property ItemFromName[const name: UnicodeString]: TQRMD3ModelItem       read GetItem;
            property FramedModelOptions:                      TQRFramedModelOptions read GetFramedModelOptions write SetFramedModelOptions;
    end;

    {**
    * Job to load MD3 from file
    *}
    TQRLoadMD3FileJob = class(TQRMD3Job)
        protected
            m_Dir:           UnicodeString;
            m_PrefixKeyword: UnicodeString;

            {**
            * Loads sub-model skin file
            *@param fileName - skin file name
            *@param pItem - item representing sub-model
            *@return true on success, otherwise false
            *}
            function LoadSkin(const fileName: TFileName;
                                 const pItem: TQRMD3ModelItem): Boolean; overload; virtual;

            {**
            * Loads animations from animation configuration file
            *@param pInfo - model group info
            *@param fileName - animation config file name
            *@return true on success, otherwise false
            *}
            function LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                 const fileName: TFileName): Boolean; overload; virtual;

            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param dir - dir in which all model files to load are contained
            *@param prefixKeyword - model templates prefix keyword
            *@param pInfo - model group info
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *}
            constructor Create(pGroup: TQRModelGroup;
                            const dir,
                        prefixKeyword: UnicodeString;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;

            {**
            * Processes the job
            *@returns true on success, otherwise false
            *}
            function Process(): Boolean; override;
    end;

    {**
    * Job to load MD3 from memory dir
    *}
    TQRLoadMD3MemoryDirJob = class(TQRMD3Job)
        protected
            m_pDir:          TQRMemoryDir;
            m_PrefixKeyword: UnicodeString;

            {**
            * Loads sub-model skin file
            *@param pStream - stream containing skin data
            *@param pItem - item representing sub-model
            *@return true on success, otherwise false
            *}
            function LoadSkin(pStream: TStream; const pItem: TQRMD3ModelItem): Boolean; overload; virtual;

            {**
            * Loads animations from animation configuration file
            *@param pInfo - model group info
            *@param pStream - buffer containing animation configuration
            *@return true on success, otherwise false
            *}
            function LoadAnimations(const pInfo: TQRMD3GroupInfo;
                                        pStream: TStream): Boolean; overload; virtual;

            {**
            * Called before a texture is loaded
            *@param pTexture - texture to load
            *@param custom - if true, texture is a custom user, otherwise a texture belonging to model
            *}
            procedure BeforeLoadTexture(pTexture: TQRTexture; custom: Boolean); override;

            {**
            * Called when a known texture should be loaded
            *@param pTexture - texture to load
            *@param pBitmap - bitmap containing loaded texture
            *@return true on success, otherwise false
            *}
            function LoadTexture(pTexture: TQRTexture;
                                  pBitmap: Vcl.Graphics.TBitmap): Boolean; override;

            {**
            * Gets the memory dir containing model files
            *@return memory dir
            *}
            function GetMemoryDir(): TQRMemoryDir; virtual;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param pDir - memory dir containing all model files to load
            *@param prefixKeyword - model templates prefix keyword
            *@param pInfo - model group info
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@throw exception if group is not defined
            *@note Memory dir will be deleted internally, do not try to delete it from outside
            *}
            constructor Create(pGroup: TQRModelGroup;
                           const pDir: TQRMemoryDir;
                  const prefixKeyword: UnicodeString;
                          const pInfo: TQRMD3GroupInfo;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;

            {**
            * Processes the job
            *@returns true on success, otherwise false
            *}
            function Process(): Boolean; override;

            { Properties }
            property MemoryDir: TQRMemoryDir read GetMemoryDir;
    end;

    {**
    * Teams at which the model belongs. Basically a .pk3 package contains 3 models versions (here
    * called teams), a default, a green and a blue. Additionally some packages may contain some
    * special packages, in this case the team name should be specified
    *}
    EQRMD3PackageTeam =
    (
        EQR_PT_MD3_Default = 0,
        EQR_PT_MD3_Red,
        EQR_PT_MD3_Blue,
        EQR_PT_MD3_Custom
    );

    {**
    * Sound files that the package may contain
    *}
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

    {**
    * Called when texture should be loaded
    *@param pGroup - group at which model belongs
    *@param pPackage - MD3 model package
    *@param [in, out] handled - if true, model will be considered as unpacked and no further
    *                           operation will be done. If false, model will be unpacked using
    *                           standard algorithm
    *@return true on success, otherwise false
    *@note Be careful, newly added files in memory dir may conflict with unpacked files while
    *      standard algorithm is applied if handled is set to false
    *}
    TQRUnpackMD3ModelEvent = function(const pGroup: TQRModelGroup;
                                          pPackage: TStream;
                                              pDir: TQRMemoryDir;
                                       var handled: Boolean): Boolean of object;

    {**
    * Job to load MD3 model from package (*.pk3 or .zip)
    *@note Some zip archives may be detected as valid but fails while stream is extracted, by
    *      returning an incoherent stream content (no error is shown when this happen). This seems
    *      to be a limitation of the zip library provided with the Embarcadero Delphi compiler (XE7),
    *      and happen sometimes with some packages created with old zippers, e.g. when
    *      RequiredVersion is set to 10 and CompressionMethod is set to 0 in the returned TZipHeader
    *      record. The solution for now is to extract and recreate the package using a recent zipper
    *}
    TQRLoadMD3PackageJob = class(TQRLoadMD3MemoryDirJob)
        protected
            m_pPackage:                TStream;
            m_pIcon:                   TBitmap;
            m_ShaderFileName:          TFileName;
            m_ExternalUnpackSucceeded: Boolean;
            m_ExternalUnpackHandled:   Boolean;
            m_fOnUnpackModel:          TQRUnpackMD3ModelEvent;

            {**
            * Unpacks model package and prepare memory directory
            *@return true on succes, otherwise false
            *}
            function Unpack(): Boolean; virtual;

            {**
            * Gets icon
            *@return icon, nil if not found or on error
            *}
            function GetIcon: TBitmap; virtual;

            {**
            * Gets shader file
            *@return shader file, nil if not found or on error
            *}
            function GetShader: TStream; virtual;

            {**
            * Gets sound
            *@param index - sound index
            *@return sound file, nil if not found or on error
            *}
            function GetSound(index: EQRMD3PackageSound): TStream; virtual;

            {**
            * Called when model is about to be unpacked externally
            *}
            procedure OnUnpackModelExternally(); virtual;

        public
            {**
            * Construction
            *@param pGroup - group that owns the job
            *@param pPackage - stream containing package model to load
            *@param prefixKeyword - model templates prefix keyword
            *@param pColor - model color
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param fOnLoadTexture - load texture callback function
            *@param team - team at which the model belongs (if available)
            *@param customTeamName - custom team name, in case the team is custom
            *@throw exception if group is not defined
            *@note Package stream will be deleted internally, do not try to delete it from outside
            *}
            constructor Create(pGroup: TQRModelGroup;
                       const pPackage: TStream;
                  const prefixKeyword: UnicodeString;
                         const pColor: TQRColor;
                         modelOptions: TQRModelOptions;
                   framedModelOptions: TQRFramedModelOptions;
                       fOnLoadTexture: TQRLoadMeshTextureEvent;
                                 team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                       customTeamName: UnicodeString = ''); reintroduce;

            {**
            * Destructor
            }
            destructor Destroy(); override;

            {**
            * Processes the job
            *@returns true on success, otherwise false
            *}
            function Process(): Boolean; override;

            { Properties }
            property Icon:                             TBitmap                read GetIcon;
            property Shader:                           TStream                read GetShader;
            property Sound[index: EQRMD3PackageSound]: TStream                read GetSound;
            property OnUnpackModel:                    TQRUnpackMD3ModelEvent read m_fOnUnpackModel write m_fOnUnpackModel;
    end;

    TQRMD3ModelPostponedGestures = TDictionary<UnicodeString, EQRMD3AnimationGesture>;

    {**
    * MD3 model group, contains all items and functions needed to manage a complete MD3 model
    *}
    TQRMD3Group = class(TQRFramedModelGroup)
        protected
            m_PrefixKeyword:      UnicodeString;                // model prefix keyword, to search and replace by final name
            m_pJob:               TQRMD3Job;                    // model job, take care of loading model inside a separated thread
            m_pPostponedGestures: TQRMD3ModelPostponedGestures; // postponed gestures, in case gesture is set while model is opening

            {**
            * Animates model
            *@param elapsedTime - elapsed time since last rendering
            *@param pItem - model item containing sub-model to animate
            *}
            procedure AnimateModel(elapsedTime: Double; pItem: TQRMD3ModelItem); virtual;

            {**
            * Gets dynamic mesh
            *@param pItem - sub-model item to get from
            *@param index - mesh index to calculate and get
            *@param[out] mesh - mesh
            *}
            procedure GetDynamicMesh(const pItem: TQRMD3ModelItem;
                                           index: NativeUInt;
                                        out mesh: TQRMesh); virtual;

            {**
            * Gets dynamic mesh, from cache if available, otherwise calculates and caches it
            *@param pItem - sub-model item to get from
            *@param index - mesh index to calculate and get
            *@param[out] mesh - mesh
            *@param[out] pTree - aligned-axis bounding box tree matching with mesh
            *}
            procedure GetDynamicMeshUseCache(const pItem: TQRMD3ModelItem;
                                                   index: NativeUInt;
                                               out pMesh: PQRMesh;
                                               out pTree: TQRAABBTree); virtual;

            {**
            * Draws dynamic model
            *@param pItem - sub-model item to draw
            *@param matrix - sub-model matrix
            *}
            procedure DrawDynamicModel(const pItem: TQRMD3ModelItem;
                                      const matrix: TQRMatrix4x4); virtual;

            {**
            * Draws cached model
            *@param pItem - sub-model item to draw
            *@param matrix - sub-model matrix
            *}
            procedure DrawCachedModel(const pItem: TQRMD3ModelItem;
                                     const matrix: TQRMatrix4x4); virtual;

            {**
            * Draws the sub-model mesh
            *@param pItem - sub-model item to draw
            *@param matrix - sub-model matrix
            *}
            procedure DrawMesh(const pItem: TQRMD3ModelItem; const matrix: TQRMatrix4x4); virtual;

            {**
            * Gets the memory dir containing model files
            *@return memory dir, nil if not found or on error
            *}
            function GetMemoryDir(): TQRMemoryDir; virtual;

            {**
            * Gets icon
            *@return icon, nil if not found or on error
            *}
            function GetIcon: TBitmap;

            {**
            * Gets shader file
            *@return shader file, nil if not found or on error
            *}
            function GetShader: TStream;

            {**
            * Gets sound
            *@param index - sound index
            *@return sound file, nil if not found or on error
            *}
            function GetSound(index: EQRMD3PackageSound): TStream;

        public
            { Construction/Destruction }
            constructor Create;  override;
            destructor  Destroy; override;

            {**
            * Clears group
            *}
            procedure Clear(); override;

            {**
            * Checks if group is empty
            *@return true if model is empty, otherwise false
            *}
            function IsEmpty(): Boolean; override;

            {**
            * Loads group from dir
            *@param dir - dir containing all model files to load
            *@param pInfo - group info
            *@param pColor - model color
            *@param rhToLh - if true, right hand coordinated will be transformed to left hand
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@return true on success, otherwise false
            *@note A standard md3 model is commonly composed of 3 sub-models, named "lower", "upper"
            *      and "head". Additionaly the model may contain a "weapon" sub-model, that should
            *      be declared explicitely in the sub-models list (to addd to the info structure
            *      Prefixes list)
            *@note A MD3 model is generally composed by the following files:
            *      - a set of .md3 files that contains the various sub-model parts
            *      - a set of .skin files, matching with each sub-models
            *      - a set of texture files, matching with each sub-models, can be of any type: bmp,
            *        jpg, pcx, ...
            *      - an animation file, that describes all the animations the model can execute,
            *        composed as follow:
            *        sex m/f
            *        [frame start] [frame count] [frame loop] [fps]
            *        0             39            0            20    // MODEL_WALK
            *        ...
            *      Inside the dir, each md3 model can be identified by a group name, e.g. to load
            *      the Lara model, the following files may be found in dir:
            *      - lara_head.md3
            *      - lara_head.skin
            *      - lara_head.jpg
            *      - lara_lower.md3
            *      - lara_lower.skin
            *      - lara_lower.jpg
            *      - ...
            *      - lara_animations.cfg
            *      The way the names are composed may vary depending of the choices made by the model
            *      creator, for this reason the info structure provides templates for file names,
            *      that may be configured as needed
            *}
            function Load(const dir: UnicodeString;
                        const pInfo: TQRMD3GroupInfo;
                       const pColor: TQRColor;
                             rhToLh: Boolean;
                       modelOptions: TQRModelOptions;
                 framedModelOptions: TQRFramedModelOptions): Boolean; overload; virtual;

            {**
            * Loads group from memory dir
            *@param pDir - memory dir containing all model streams to load
            *@param pInfo - group info
            *@param pColor - model color
            *@param rhToLh - if true, right hand coordinated will be transformed to left hand
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@return true on success, otherwise false
            *@note Memory dir will be deleted internally, do not try to delete it from outside
            *@note A standard md3 model is commonly composed of 3 sub-models, named "lower", "upper"
            *      and "head". Additionaly the model may contain a "weapon" sub-model, that should
            *      be declared explicitely in the sub-models list (to addd to the info structure
            *      Prefixes list)
            *@note A MD3 model is generally composed by the following files:
            *      - a set of .md3 files that contains the various sub-model parts
            *      - a set of .skin files, matching with each sub-models
            *      - a set of texture files, matching with each sub-models, can be of any type: bmp,
            *        jpg, pcx, ...
            *      - an animation file, that describes all the animations the model can execute,
            *        composed as follow:
            *        sex m/f
            *        [frame start] [frame count] [frame loop] [fps]
            *        0             39            0            20    // MODEL_WALK
            *        ...
            *      Inside the dir, each md3 model can be identified by a group name, e.g. to load
            *      the Lara model, the following files may be found in dir:
            *      - lara_head.md3
            *      - lara_head.skin
            *      - lara_head.jpg
            *      - lara_lower.md3
            *      - lara_lower.skin
            *      - lara_lower.jpg
            *      - ...
            *      - lara_animations.cfg
            *      The way the names are composed may vary depending of the choices made by the model
            *      creator, for this reason the info structure provides templates for file names,
            *      that may be configured as needed
            *}
            function Load(const pDir: TQRMemoryDir;
                         const pInfo: TQRMD3GroupInfo;
                        const pColor: TQRColor;
                              rhToLh: Boolean;
                        modelOptions: TQRModelOptions;
                  framedModelOptions: TQRFramedModelOptions): Boolean; overload; virtual;

            {**
            * Loads group from package (.pk3 or .zip) file
            *@param fileName - model package file name to load
            *@param pColor - model color
            *@param rhToLh - if true, right hand coordinated will be transformed to left hand
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param team - team at which the model belongs (if available)
            *@param customTeamName - custom team name, in case the team is custom
            *@return true on success, otherwise false
            *}
            function Load(const fileName: TFileName;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                          customTeamName: UnicodeString = ''): Boolean; overload; virtual;

            {**
            * Loads group from package (.pk3 or .zip) stream
            *@param pPackage - stream containing model package to load
            *@param pColor - model color
            *@param rhToLh - if true, right hand coordinated will be transformed to left hand
            *@param modelOptions - model options to apply
            *@param framedModelOptions - framed model options to apply
            *@param team - team at which the model belongs (if available)
            *@param customTeamName - custom team name, in case the team is custom
            *@return true on success, otherwise false
            *@note Package stream will be deleted internally, do not try to delete it from outside
            *}
            function Load(const pPackage: TStream;
                            const pColor: TQRColor;
                                  rhToLh: Boolean;
                            modelOptions: TQRModelOptions;
                      framedModelOptions: TQRFramedModelOptions;
                                    team: EQRMD3PackageTeam = EQR_PT_MD3_Default;
                          customTeamName: UnicodeString = ''): Boolean; overload; virtual;

            {**
            * Selects animation to run for a sub-model
            *@param name - sub-model name
            *@param gesture - animation gesture to execute
            *@return true on success, otherwise false
            *}
            function SetAnimation(const name: UnicodeString;
                                     gesture: EQRMD3AnimationGesture): Boolean; virtual;

            {**
            * Queries the job status
            *@return job status
            *}
            function QueryJobStatus(): TQRModelJobStatus; override;

            {**
            * Draws the group
            *@param elapsedTime - elapsed time since last draw
            *}
            procedure Draw(const elapsedTime: Double); override;

            { Properties }
            property MemoryDir:                        TQRMemoryDir read GetMemoryDir;
            property Icon:                             TBitmap      read GetIcon;
            property Shader:                           TStream      read GetShader;
            property Sound[index: EQRMD3PackageSound]: TStream      read GetSound;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRMD3Helper
//--------------------------------------------------------------------------------------------------
constructor TQRMD3Helper.Create();
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Helper.Destroy();
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
    begin
        Result := prefix;
        Exit;
    end;

    // build name from template
    Result := StringReplace(templateName, prefixKeyword, prefix, []);
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
constructor TQRMD3AnimCfgFile.Create();
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
destructor TQRMD3AnimCfgFile.Destroy();
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
    i, index: NativeUInt;
    gesture:  NativeInt;
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
        begin
            Result := False;
            Exit;
        end;
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
        begin
            Result := False;
            Exit;
        end;

        // by default, each line contains 4 numeric values, that describes the animation
        for i := 1 to Length(word) do
            if ((word[i] <> '\0') and (not TQRStringHelper.IsNumeric(word[i], false))) then
            begin
                Result := False;
                Exit;
            end;

        // search for head offset value to set
        case (m_Column) of
            0: m_HeadOffset.m_UnknownOffset1 := StrToInt(word);
            1: m_HeadOffset.m_UnknownOffset2 := StrToInt(word);
            2: m_HeadOffset.m_UnknownOffset3 := StrToInt(word);
        else
            Result := False;
            Exit;
        end;

        Inc(m_Column);
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
        begin
            Result := False;
            Exit;
        end;

        // by default, each line contains 4 numeric values, that describes the animation
        for i := 1 to Length(word) do
            if ((word[i] <> '\0') and (not TQRStringHelper.IsNumeric(word[i], false))) then
            begin
                Result := False;
                Exit;
            end;

        // first item to parse?
        if (Length(m_Items) = 0) then
        begin
            // store the start line, it will be used later to find the animation type
            m_StartLine := lineNb;
            m_CurLine   := lineNb;
        end;

        // get current item index
        index := Length(m_Items);

        // first animation value?
        if (m_Column = 0) then
        begin
            // get animation gesture (each line pos in file matchs with the IEGesture enumerator)
            gesture := (m_CurLine - m_StartLine);

            // invalid type?
            if (gesture >= NativeInt(EQR_AG_MD3_Max_Animations)) then
            begin
                Result := False;
                Exit;
            end;

            // create and populate new item, and add it to list
            SetLength(m_Items, index + 1);
            m_Items[index].m_Gesture := gesture;

            Inc(m_CurLine);
        end
        else
            Dec(index);

        // search for animation item value to set
        case (m_Column) of
            0: m_Items[index].m_StartFrame      := StrToInt(word);
            1: m_Items[index].m_FrameCount      := StrToInt(word);
            2: m_Items[index].m_LoopingFrames   := StrToInt(word);
            3: m_Items[index].m_FramesPerSecond := StrToInt(word);
        else
            Result := False;
            Exit;
        end;

        Inc(m_Column);
    end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3AnimCfgFile.Clear();
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
function TQRMD3AnimCfgFile.GetGender(): EQRMD3Gender;
begin
    Result := m_Gender;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.GetHeadOffset(): PQRMD3AnimCfgFileHeadOffset;
begin
    Result := @m_HeadOffset;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3AnimCfgFile.GetFootSteps(): PQRMD3AnimCfgFileFootSteps;
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
constructor TQRMD3Skin.Create();
begin
    inherited Create;

    m_PathTable := TQRMD3PathTable.Create;
    m_LinkKeys  := TQRMD3LinkKeys.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRMD3Skin.Destroy();
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
    readPath:   Boolean;
    i:          NativeUInt;
begin
    // no line to parse?
    if (Length(line) = 0) then
    begin
        Result := True;
        Exit;
    end;

    readPath := False;

    // iterate through line chars
    for i := 1 to Length(line) do
    begin
        // dispatch char
        case (line[i]) of
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
                path := path + line[i];
                continue;
            end;

            // add char to name
            name := name + line[i];
        end;
    end;

    // empty name?
    if (Length(name) = 0) then
    begin
        Result := False;
        Exit;
    end;

    // no path?
    if (Length(path) = 0) then
    begin
        // in this case, the line contains a link key. Check if the name key already exists
        if (m_LinkKeys.IndexOf(name) <> -1) then
        begin
            Result := False;
            Exit;
        end;

        // add link key to table
        m_LinkKeys.Add(name);

        Result := True;
        Exit;
    end;

    // name already exists in path table?
    if (m_PathTable.ContainsKey(name)) then
    begin
        Result := False;
        Exit;
    end;

    // add name and path in table
    m_PathTable.Add(name, path);

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Skin.Clear();
begin
    m_PathTable.Clear();
    m_LinkKeys.Clear();
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Skin.GetPath(const name: UnicodeString): UnicodeString;
begin
    // name exists in table?
    if (not m_PathTable.ContainsKey(name)) then
    begin
        Result := '';
        Exit;
    end;

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
    i:        Cardinal;
    animItem: TPair<EQRMD3AnimationGesture, TQRMD3AnimationItem>;
begin
    // clear animations
    for animItem in m_pAnimations do
        animItem.Value.Free;

    m_pAnimations.Free;

    // clear textures
    if (Length(m_Textures) > 0) then
        for i := 0 to Length(m_Textures) - 1 do
            m_Textures[i].Free;

    SetLength(m_Textures, 0);

    // clear links from
    if (Length(m_LinksFrom) > 0) then
        for i := 0 to Length(m_LinksFrom) - 1 do
            m_LinksFrom[i].Free;

    SetLength(m_LinksFrom, 0);

    // clear links to
    if (Length(m_LinksTo) > 0) then
        for i := 0 to Length(m_LinksTo) - 1 do
            m_LinksTo[i].Free;

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
    linkCount, i: NativeInt;
    index:        NativeUInt;
begin
    // get link count
    linkCount := Length(links);

    // iterate through existing links
    if (linkCount > 0) then
        for i := 0 to linkCount - 1 do
            // link already added?
            if (links[i].m_pItem = pTarget) then
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
    begin
        Result := nil;
        Exit;
    end;

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
    begin
        Result := nil;
        Exit;
    end;

    Result := m_Textures[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinkFrom(index: NativeInt): TQRMD3ModelLink;
begin
    if (index >= Length(m_LinksFrom)) then
    begin
        Result := nil;
        Exit;
    end;

    Result := m_LinksFrom[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3ModelItem.GetLinkTo(index: NativeInt): TQRMD3ModelLink;
begin
    if (index >= Length(m_LinksTo)) then
    begin
        Result := nil;
        Exit;
    end;

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
destructor TQRMD3GroupInfo.Destroy();
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
    begin
        Result := '';
        Exit;
    end;

    Result := m_pPrefixes[index];
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3GroupInfo.GetPrefixCount(): NativeInt;
begin
    Result := m_pPrefixes.Count;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3GroupInfo.AddPrefix(const prefix: UnicodeString);
var
    index: Integer;
begin
    // prefix already exists in list?
    if (m_pPrefixes.Find(prefix, index)) then
        Exit;

    m_pPrefixes.Add(prefix);
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
destructor TQRMD3Job.Destroy();
var
    i: NativeInt;
begin
    m_pLock.Lock;

    try
        // clear items
        if (Length(m_Items) > 0) then
            for i := 0 to Length(m_Items) - 1 do
                m_Items[i].Free;

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
        begin
            Result := nil;
            Exit;
        end;

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
        begin
            Result := nil;
            Exit;
        end;

        Result := m_pItemDictionary[name]^;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.GetFramedModelOptions(): TQRFramedModelOptions;
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
procedure TQRMD3Job.LinkModel();
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
    begin
        Result := False;
        Exit;
    end;

    // item contains no model?
    if (not Assigned(pItem.m_pModel)) then
    begin
        Result := False;
        Exit;
    end;

    // get model parser
    pParser := pItem.m_pModel.Parser;

    // found it?
    if (not Assigned(pParser)) then
    begin
        Result := False;
        Exit;
    end;

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
            begin
                Result := False;
                Exit;
            end;
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
            begin
                Result := False;
                Exit;
            end;
        end;

    Result := True;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.LinkAnimations(const pInfo: TQRMD3GroupInfo;
                            const pAnimations: TQRMD3AnimCfgFile): Boolean;
var
    animCount, lowerDelta, i, j: NativeUInt;
    pItem, pStartItem, pEndItem: PQRModelAnimCfgItem;
    pAnimItem:                   TQRMD3AnimationItem;
begin
    animCount  := pAnimations.GetItemCount();
    lowerDelta := 0;

    if (animCount = 0) then
    begin
        Result := True;
        Exit;
    end;

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
            if (pInfo.m_pPrefixes.Count > 0) then
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
            if (pInfo.m_pPrefixes.Count > 0) then
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
                begin
                    Result := False;
                    Exit;
                end;

                // calculate animation delta to apply to lower model part
                lowerDelta := pEndItem.m_StartFrame - pStartItem.m_StartFrame;
            end;

            // iterate through model parts
            if (pInfo.m_pPrefixes.Count > 0) then
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
procedure TQRMD3Job.OnLoadTexture();
var
    max:                 NativeUInt;
    loadFirst, loadNext: Boolean;
    pModelTexture:       TQRTexture;
    pTexture:            Vcl.Graphics.TBitmap;
    pItem:               TQRMD3ModelItem;
begin
    m_pLock.Lock;

    try
        m_TextureLoaded := False;

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

                // do load texture for the first time? (NOTE , so only try to load the first texture, others are user
                // defined textures)
                if (loadFirst) then
                begin
                    pTexture := Vcl.Graphics.TBitmap.Create;

                    try
                        // load texture
                        if (LoadTexture(pModelTexture, pTexture)) then
                        begin
                            // notify that a texture is loading
                            if (Assigned(m_fOnLoadTexture)) then
                                if (not m_fOnLoadTexture(m_pGroup,
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
                            if (not m_fOnLoadTexture(m_pGroup,
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
                        if (not m_fOnLoadTexture(m_pGroup,
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
procedure TQRMD3Job.Cancel();
begin
    m_pLock.Lock;
    m_IsCanceled := True;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Job.IsCanceled(): Boolean;
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
destructor TQRLoadMD3FileJob.Destroy();
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.LoadSkin(const fileName: TFileName;
                                       const pItem: TQRMD3ModelItem): Boolean;
begin
    // load skin file
    if (not pItem.m_pSkin.Load(fileName)) then
    begin
        Result := False;
        Exit;
    end;

    Result := LinkSkin(pItem.m_pSkin,
                       TQRFileHelper.AppendDelimiter(ExtractFileDir(fileName)),
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
        begin
            Result := False;
            Exit;
        end;

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
        pTexture.FileName := m_TextureFileName;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.LoadTexture(pTexture: TQRTexture;
                                        pBitmap: Vcl.Graphics.TBitmap): Boolean;
var
    fileName:      TFileName;
    pFileStream:   TFileStream;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // build texture file name
    fileName := TQRFileHelper.AppendDelimiter(pTexture.Dir) + pTexture.FileName;

    // check if texture file exists
    if (not FileExists(fileName)) then
    begin
        Result := False;
        Exit;
    end;

    // load image file in a stream
    pFileStream := TFileStream.Create(fileName, fmOpenRead);

    try
        // found it?
        if (not Assigned(pFileStream)) then
        begin
            Result := False;
            Exit;
        end;

        // load texture
        Result := TQRModelGroupHelper.LoadTexture(pFileStream,
                                                  ExtractFileExt(pTexture.FileName),
                                                  pBitmap);
    finally
        // clear memory
        pFileStream.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3FileJob.Process(): Boolean;
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
    // if job was still loaded, don't reload it. A such scenario can happen when a job is deleted in
    // the job list. In this case, all jobs are removed from list, the concerned job is deleted,
    // then all remaining jobs are added back, calling thus the Process() function again
    if (m_IsLoaded) then
    begin
        Result := True;
        Exit;
    end;

    try
        m_pLock.Lock;
        m_Progress := 0.0;
        m_pLock.Unlock;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in m_ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in m_ModelOptions));

        // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
        // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
        // (except for optional files) begins with the same prefix, that is usually head, upper or lower
        subModelGroupCount := m_pInfo.m_pPrefixes.Count;

        // iterate through sub-models to load
        if (subModelGroupCount = 0) then
        begin
            Result := False;
            Exit;
        end;

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
                m_Items[i].m_Name := m_pInfo.m_pPrefixes[i];
                m_ItemIndex       := i;
            finally
                m_pLock.Unlock;
            end;

            // build model name
            modelName := TQRMD3Helper.BuildName(m_pInfo.m_ModelTemplate,
                                                m_PrefixKeyword,
                                                m_pInfo.m_pPrefixes[i]);

            // build model file name
            modelFileName := TQRFileHelper.AppendDelimiter(m_Dir) + modelName + '.md3';

            // model file exists?
            if (not FileExists(modelFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 model file not exists - ' +
                                               modelFileName                  +
                                               ' - class name - '             +
                                               ClassName);
                {$endif}

                Result := False;
                Exit;
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

                Result := False;
                Exit;
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
            m_pLock.Lock;
            m_Progress := m_Progress + progressStep;
            m_pLock.Unlock;

            // set model color
            m_Items[i].m_pModel.Color := m_pColor;

            // build skin file name
            skinFileName := TQRFileHelper.AppendDelimiter(m_Dir)           +
                            TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                                   m_PrefixKeyword,
                                                   m_pInfo.m_pPrefixes[i]) +
                            '.skin';

            // skin file exists?
            if (not FileExists(skinFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 skin file not exists - ' +
                                               skinFileName                  +
                                               ' - class name - '            +
                                               ClassName);
                {$endif}

                Result := False;
                Exit;
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

                Result := False;
                Exit;
            end;

            m_pLock.Lock;

            try
                textureLoaded := m_TextureLoaded;

                // skin is loaded, add one step to progress
                m_Progress := m_Progress + progressStep;
            finally
                m_pLock.Unlock;
            end;

            // do include colors?
            if (EQR_MO_Without_Colors in m_ModelOptions) then
                vertexFormat := []
            else
                vertexFormat := [EQR_VF_Colors];

            // normals loaded?
            if (not(EQR_MO_Without_Normals in m_ModelOptions)) then
                Include(vertexFormat, EQR_VF_Normals);

            // texture loaded?
            if (textureLoaded and (not(EQR_MO_Without_Textures in m_ModelOptions))) then
                Include(vertexFormat, EQR_VF_TexCoords);

            // set vertex format
            m_Items[i].m_pModel.VertexFormat := vertexFormat;

            // add newly created item to dictionary (to facilitate search later)
            m_pItemDictionary.AddOrSetValue(modelName, @m_Items[i]);

            // model is configured and default mesh is created, add one step to progress
            m_pLock.Lock;
            m_Progress := m_Progress + progressStep;
            m_pLock.Unlock;

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
                    if (not(EQR_MO_No_Collision in m_ModelOptions)) then
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
                        Result := False;
                        Exit;
                    end;

                    // add mesh to cache, note that from now cache will take care of the pointer
                    try
                        m_pCache.Mesh[m_Items[i].m_CacheIndex + j] := pMesh;
                    except
                        Dispose(pMesh);
                    end;

                    // do ignore collisions?
                    if (not(EQR_MO_No_Collision in m_ModelOptions)) then
                        // add tree to cache, note that from now cache will take care of the pointer
                        try
                            m_pCache.AABBTree[m_Items[i].m_CacheIndex + j] := pTree;
                        except
                            pTree.Free;
                        end;

                    // update next available cache index position
                    Inc(cacheIndex);

                    // a new frame was cached, add one step to progress
                    m_pLock.Lock;
                    m_Progress := m_Progress + progressStep;
                    m_pLock.Unlock;
                end;
            end;
        end;

        LinkModel();

        // calculate the value of a progress step
        progressStep := (totalItemStep / 2.0);

        // model was linked, add one step to progress
        m_pLock.Lock;
        m_Progress := m_Progress + progressStep;
        m_pLock.Unlock;

        // build animation file name
        animFileName :=
                TQRFileHelper.AppendDelimiter(m_Dir)                                         +
                                              TQRMD3Helper.BuildName(m_pInfo.m_AnimTemplate,
                                                                     m_PrefixKeyword,
                                                                     'animation')            +
                                              '.cfg';

        // open it
        if (not LoadAnimations(m_pInfo, animFileName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('Failed to load MD3 animation - file name - ' +
                                           animFileName                                  +
                                           ' - class name - '                            +
                                           ClassName);
            {$endif}

            Result := False;
            Exit;
        end;

        // model is fully loaded
        m_pLock.Lock;
        m_Progress := 100.0;
        m_pLock.Unlock;

        m_IsLoaded := True;
        Result     := True;
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
destructor TQRLoadMD3MemoryDirJob.Destroy();
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
    begin
        Result := False;
        Exit;
    end;

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
        begin
            Result := False;
            Exit;
        end;

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
        pTexture.FileName := m_TextureFileName;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.LoadTexture(pTexture: TQRTexture;
                                             pBitmap: Vcl.Graphics.TBitmap): Boolean;
var
    pImageStream: TStream;
begin
    // no texture bitmap to load to?
    if (not Assigned(pBitmap)) then
    begin
        Result := False;
        Exit;
    end;

    // file exists in memory dir?
    if (not m_pDir.FileExists(pTexture.FileName)) then
    begin
        Result := False;
        Exit;
    end;

    // get image stream
    pImageStream := m_pDir.GetFile(pTexture.FileName);

    // found it?
    if (not Assigned(pImageStream)) then
    begin
        Result := False;
        Exit;
    end;

    pImageStream.Position := 0;

    // load texture
    Result := TQRModelGroupHelper.LoadTexture(pImageStream,
                                              ExtractFileExt(pTexture.FileName),
                                              pBitmap);
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.GetMemoryDir(): TQRMemoryDir;
begin
    m_pLock.Lock;
    Result := m_pDir;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3MemoryDirJob.Process(): Boolean;
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
    // if job was still loaded, don't reload it. A such scenario can happen when a job is deleted in
    // the job list. In this case, all jobs are removed from list, the concerned job is deleted,
    // then all remaining jobs are added back, calling thus the Process() function again
    if (m_IsLoaded) then
    begin
        Result := True;
        Exit;
    end;

    try
        m_pLock.Lock;
        m_Progress := 0.0;
        m_pLock.Unlock;

        // check if cache should be created
        doCreateCache := ((EQR_MO_Create_Cache   in m_ModelOptions) and
                      not (EQR_MO_Dynamic_Frames in m_ModelOptions));

        // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
        // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
        // (except for optional files) begins with the same prefix, that is usually head, upper or lower
        subModelGroupCount := m_pInfo.m_pPrefixes.Count;

        // iterate through sub-models to load
        if (subModelGroupCount = 0) then
        begin
            Result := False;
            Exit;
        end;

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
                m_Items[i].m_Name := m_pInfo.m_pPrefixes[i];
                m_ItemIndex       := i;
            finally
                m_pLock.Unlock;
            end;

            // build model name
            modelName := TQRMD3Helper.BuildName(m_pInfo.m_ModelTemplate,
                                                m_PrefixKeyword,
                                                m_pInfo.m_pPrefixes[i]);

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

                Result := False;
                Exit;
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

                Result := False;
                Exit;
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

                Result := False;
                Exit;
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
            m_pLock.Lock;
            m_Progress := m_Progress + progressStep;
            m_pLock.Unlock;

            // set model color
            m_Items[i].m_pModel.Color := m_pColor;

            // build skin file name
            skinFileName := TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                                   m_PrefixKeyword,
                                                   m_pInfo.m_pPrefixes[i]) + '.skin';

            // skin file exists?
            if (not m_pDir.FileExists(skinFileName)) then
            begin
                {$ifdef DEBUG}
                    TQRLogHelper.LogToCompiler('MD3 skin stream does not exist - ' +
                                               skinFileName                        +
                                               ' - class name - '                  +
                                               ClassName);
                {$endif}

                Result := False;
                Exit;
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

                Result := False;
                Exit;
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

                Result := False;
                Exit;
            end;

            m_pLock.Lock;

            try
                textureLoaded := m_TextureLoaded;

                // skin is loaded, add one step to progress
                m_Progress := m_Progress + progressStep;
            finally
                m_pLock.Unlock;
            end;

            // do include colors?
            if (EQR_MO_Without_Colors in m_ModelOptions) then
                vertexFormat := []
            else
                vertexFormat := [EQR_VF_Colors];

            // normals loaded?
            if (not(EQR_MO_Without_Normals in m_ModelOptions)) then
                Include(vertexFormat, EQR_VF_Normals);

            // texture loaded?
            if (textureLoaded and (not(EQR_MO_Without_Textures in m_ModelOptions))) then
                Include(vertexFormat, EQR_VF_TexCoords);

            // set vertex format
            m_Items[i].m_pModel.VertexFormat := vertexFormat;

            // add newly created item to dictionary (to facilitate search later)
            m_pItemDictionary.AddOrSetValue(modelName, @m_Items[i]);

            // model is configured and default mesh is created, add one step to progress
            m_pLock.Lock;
            m_Progress := m_Progress + progressStep;
            m_pLock.Unlock;

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
                    if (not(EQR_MO_No_Collision in m_ModelOptions)) then
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
                        Result := False;
                        Exit;
                    end;

                    // add mesh to cache, note that from now cache will take care of the pointer
                    try
                        m_pCache.Mesh[m_Items[i].m_CacheIndex + j] := pMesh;
                    except
                        Dispose(pMesh);
                    end;

                    // do ignore collisions?
                    if (not(EQR_MO_No_Collision in m_ModelOptions)) then
                        // add tree to cache, note that from now cache will take care of the pointer
                        try
                            m_pCache.AABBTree[m_Items[i].m_CacheIndex + j] := pTree;
                        except
                            pTree.Free;
                        end;

                    // update next available cache index position
                    Inc(cacheIndex);

                    // a new frame was cached, add one step to progress
                    m_pLock.Lock;
                    m_Progress := m_Progress + progressStep;
                    m_pLock.Unlock;
                end;
            end;
        end;

        LinkModel();

        // calculate the value of a progress step
        progressStep := (totalItemStep / 2.0);

        // model was linked, add one step to progress
        m_pLock.Lock;
        m_Progress := m_Progress + progressStep;
        m_pLock.Unlock;

        // build animation file name
        animFileName := TQRMD3Helper.BuildName(m_pInfo.m_AnimTemplate,
                                               m_PrefixKeyword,
                                               'animation') + '.cfg';

        // animation file exists?
        if (not m_pDir.FileExists(animFileName)) then
        begin
            {$ifdef DEBUG}
                TQRLogHelper.LogToCompiler('MD3 animation stream does not exist - ' +
                                           animFileName                             +
                                           ' - class name - '                       +
                                           ClassName);
            {$endif}

            Result := False;
            Exit;
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

            Result := False;
            Exit;
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

            Result := False;
            Exit;
        end;

        // model is fully loaded
        m_pLock.Lock;
        m_Progress := 100.0;
        m_pLock.Unlock;

        m_IsLoaded := True;
        Result     := True;
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
destructor TQRLoadMD3PackageJob.Destroy();
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
function TQRLoadMD3PackageJob.Unpack(): Boolean;
var
    pZipFile:     TZipFile;
    pLocalHeader: TZipHeader;
    fileName:     TFileName;
    pZipStream:   TStream;
    pFileStream:  TMemoryStream;
    i:            NativeUInt;
begin
    // no stream to load to?
    if (not Assigned(m_pPackage)) then
    begin
        Result := False;
        Exit;
    end;

    try
        // do unpack externally?
        if (Assigned(m_fOnUnpackModel)) then
        begin
            TThread.Synchronize(nil, OnUnpackModelExternally);

            // external unpack failed?
            if (not m_ExternalUnpackSucceeded) then
            begin
                Result := False;
                Exit;
            end;

            // external unpack was handled?
            if (m_ExternalUnpackHandled) then
            begin
                Result := True;
                Exit;
            end;
        end;

        // create zipper instance
        pZipFile := TZipFile.Create;

        try
            // open zip file for read
            pZipFile.Open(m_pPackage, zmRead);

            // iterate through zipped files
            for i := 0 to pZipFile.FileCount - 1 do
            begin
                // get next zipped file name (in lower case and without path)
                fileName := LowerCase(TQRFileHelper.ExtractFileName(pZipFile.FileNames[i],
                                                                    CQR_Zip_Dir_Delimiter));

                // found a dir? (in this case file name cannot be found)
                if (Length(fileName) = 0) then
                    continue;

                // file already exists in memory dir?
                if (m_pDir.FileExists(fileName)) then
                begin
                    {$ifdef DEBUG}
                        TQRLogHelper.LogToCompiler('MD3 - unpack - found duplicate - file should be unique in package - ' +
                                                   fileName                                                               +
                                                   ' - class name - '                                                     +
                                                   ClassName);
                    {$endif}

                    Result := False;
                    Exit;
                end;

                pZipStream := nil;

                try
                    // extract file from zip
                    pZipFile.Read(pZipFile.FileNames[i], pZipStream, pLocalHeader);

                    // succeeded?
                    if (not Assigned(pZipStream)) then
                    begin
                        {$ifdef DEBUG}
                            TQRLogHelper.LogToCompiler('MD3 - unpack - failed to extract stream from zip - ' +
                                                       fileName                                              +
                                                       ' - class name - '                                    +
                                                       ClassName);
                        {$endif}

                        Result := False;
                        Exit;
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

                // found shader file?
                if (ExtractFileExt(fileName) = '.shader') then
                    // keep file name
                    m_ShaderFileName := fileName;
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
        begin
            Result := nil;
            Exit;
        end;

        // icon was already loaded?
        if ((m_pIcon.Width > 0) and (m_pIcon.Height > 0)) then
        begin
            Result := m_pIcon;
            Exit;
        end;

        index := 0;

        repeat
            // get next image extension to load
            extension := m_TextureExt[index];

            // build icon file name
            fileName := TQRMD3Helper.BuildName(m_pInfo.m_SkinTemplate,
                                               m_PrefixKeyword,
                                               'icon') + extension;

            // check if icon file exists
            iconExists := m_pDir.FileExists(fileName);

            Inc(index);
        until (iconExists or (index >= Length(m_TextureExt)));

        // found a icon file to load?
        if (not iconExists) then
        begin
            Result := nil;
            Exit;
        end;

        // get icon stream
        pIconStream := m_pDir.GetFile(fileName);

        // found it?
        if (not Assigned(pIconStream)) then
        begin
            Result := nil;
            Exit;
        end;

        // load texture
        if (not TQRModelGroupHelper.LoadTexture(pIconStream, extension, m_pIcon)) then
        begin
            Result := nil;
            Exit;
        end;

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
        begin
            Result := nil;
            Exit;
        end;

        // no shader file?
        if (Length(m_ShaderFileName) = 0) then
        begin
            Result := nil;
            Exit;
        end;

        // file exists?
        if (not m_pDir.FileExists(m_ShaderFileName)) then
        begin
            Result := nil;
            Exit;
        end;

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
        begin
            Result := nil;
            Exit;
        end;

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
        begin
            Result := nil;
            Exit;
        end;

        // file exists?
        if (not m_pDir.FileExists(fileName)) then
        begin
            Result := nil;
            Exit;
        end;

        // get file
        Result := m_pDir.GetFile(fileName);
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRLoadMD3PackageJob.OnUnpackModelExternally();
begin
    m_pLock.Lock;

    try
        // no event defined?
        if (not Assigned(m_fOnUnpackModel)) then
        begin
            m_ExternalUnpackSucceeded := False;
            Exit;
        end;

        // call event
        m_ExternalUnpackHandled   := True;
        m_ExternalUnpackSucceeded := m_fOnUnpackModel(m_pGroup,
                                                      m_pPackage,
                                                      m_pDir,
                                                      m_ExternalUnpackHandled);

    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRLoadMD3PackageJob.Process(): Boolean;
begin
    // if job was still loaded, don't reload it. A such scenario can happen when a job is deleted in
    // the job list. In this case, all jobs are removed from list, the concerned job is deleted,
    // then all remaining jobs are added back, calling thus the Process() function again
    if (m_IsLoaded) then
    begin
        Result := True;
        Exit;
    end;

    m_pLock.Lock;
    m_Progress := 0.0;
    m_pLock.Unlock;

    // unpack model package
    if (not Unpack()) then
    begin
        Result := False;
        Exit;
    end;

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
    if (not Assigned(m_fOnDrawItem)) then
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
            m_fOnDrawItem(Self,
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
                m_fOnDrawItem(Self,
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
        m_fOnDrawItem(Self,
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
        m_fOnDrawItem(Self,
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
    if (not Assigned(m_fOnDrawItem)) then
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
            m_fOnDrawItem(Self,
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
            m_fOnDrawItem(Self,
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
        m_fOnDrawItem(Self,
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
        m_fOnDrawItem(Self,
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
    linkCount, i:                     NativeUInt;
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
    if (Assigned(m_fOnCustomDrawItem))
    then
        // let user take care of drawing model
        m_fOnCustomDrawItem(Self,
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

    // get model tag count
    linkCount := Length(pItem.m_LinksTo);

    // no links?
    if (linkCount = 0) then
        Exit;

    // iterate through model tags
    for i := 0 to linkCount - 1 do
    begin
        // get sub-model link
        pLink := pItem.m_LinksTo[i];

        // found it?
        if (not Assigned(pLink)) then
            Exit;

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
        interpolatedMatrix := interpolatedQuat.GetMatrix();

        // set interpolated position inside resulting matrix directly
        interpolatedMatrix.Table[3, 0] := pos.X;
        interpolatedMatrix.Table[3, 1] := pos.Y;
        interpolatedMatrix.Table[3, 2] := pos.Z;

        // draw next linked sub-model
        DrawMesh(pLink.m_pItem, interpolatedMatrix.Multiply(matrix));
    end;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetMemoryDir(): TQRMemoryDir;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        Result := nil;
        Exit;
    end;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        Result := nil;
        Exit;
    end;

    // is job a memory dir job?
    if (m_pJob is TQRLoadMD3MemoryDirJob) then
    begin
        // get and return memory dir
        Result := TQRLoadMD3MemoryDirJob(m_pJob).MemoryDir;
        Exit;
    end;

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetIcon: TBitmap;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        Result := nil;
        Exit;
    end;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        Result := nil;
        Exit;
    end;

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
    begin
        // get and return icon
        Result := TQRLoadMD3PackageJob(m_pJob).Icon;
        Exit;
    end;

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetShader: TStream;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        Result := nil;
        Exit;
    end;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        Result := nil;
        Exit;
    end;

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
    begin
        // get and return shader file
        Result := TQRLoadMD3PackageJob(m_pJob).Shader;
        Exit;
    end;

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
function TQRMD3Group.GetSound(index: EQRMD3PackageSound): TStream;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        Result := nil;
        Exit;
    end;

    // model still loading?
    if (m_pJob.GetStatus <> EQR_JS_Done) then
    begin
        Result := nil;
        Exit;
    end;

    // is job a package job?
    if (m_pJob is TQRLoadMD3PackageJob) then
    begin
        // get and return sound
        Result := TQRLoadMD3PackageJob(m_pJob).Sound[index];
        Exit;
    end;

    Result := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.Clear();
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
function TQRMD3Group.IsEmpty(): Boolean;
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
    Clear();

    // prepare model job to load from file
    m_pJob := TQRLoadMD3FileJob.Create(Self,
                                       dir,
                                       m_PrefixKeyword,
                                       pInfo,
                                       pColor,
                                       modelOptions,
                                       framedModelOptions,
                                       m_fOnLoadTexture);

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
        Clear();
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
                                            m_fOnLoadTexture);

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
    begin
        Result := False;
        Exit;
    end;

    // is file a valid zip package?
    if (not TZipFile.IsValid(fileName)) then
    begin
        Result := False;
        Exit;
    end;

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
    begin
        Result := False;
        Exit;
    end;

    try
        // clear previous group instance
        Clear();
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
                                          m_fOnLoadTexture,
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
        Result := True;
        Exit;
    end;

    // animation dictionary contains sub-model for which animation should be changed?
    if (not m_pJob.m_pItemDictionary.ContainsKey(name)) then
    begin
        Result := False;
        Exit;
    end;

    // get sub-model item
    pItem := m_pJob.m_pItemDictionary[name]^;

    // found it?
    if (not Assigned(pItem)) then
    begin
        Result := False;
        Exit;
    end;

    // is gesture supported by sub-model?
    if (not pItem.m_pAnimations.ContainsKey(gesture)) then
    begin
        Result := False;
        Exit;
    end;

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
function TQRMD3Group.QueryJobStatus(): TQRModelJobStatus;
begin
    // model not created?
    if (not Assigned(m_pJob)) then
    begin
        // set default values
        m_pJobStatus.Status   := EQR_JS_NotStarted;
        m_pJobStatus.Progress := 0;
    end
    else
    begin
        // get status from running job
        m_pJobStatus.Status   := m_pJob.GetStatus;
        m_pJobStatus.Progress := Floor(m_pJob.Progress);
    end;

    Result := m_pJobStatus;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRMD3Group.Draw(const elapsedTime: Double);
var
    itemCount, i:         NativeUInt;
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

    // get sub-model item count
    itemCount := Length(m_pJob.m_Items);

    // no sub-models to draw?
    if (itemCount = 0) then
        Exit;

    // animate each sub-model
    for i := 0 to itemCount - 1 do
        AnimateModel(elapsedTime, m_pJob.m_Items[i]);

    // get model matrix
    modelMatrix := GetMatrix();

    // draw all sub-models, recursively
    DrawMesh(m_pJob.m_Items[0], modelMatrix);
end;
//--------------------------------------------------------------------------------------------------

end.
