/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 normals table file generator main interface              *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Imaging.pngimage.hpp>

// std
#include <string>

/**
* MD2 normals table file generator main interface
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TTimer *tiAnimate;
        TPanel *paTitle;
        TLabel *laDetails;
        TSaveDialog *sdSaveDialog;
        TEdit *edFileName;
        TButton *btBrowse;
        TButton *btGenerate;
        TPanel *paFileName;
        TPanel *paHeader;
        TImage *imIcon;
        TMemo *meLog;

        void __fastcall tiAnimateTimer(TObject* pSender);
        void __fastcall btBrowseClick(TObject* pSender);
        void __fastcall btGenerateClick(TObject* pSender);

    public:
        __fastcall TMainForm(TComponent* pOwner);

    private:
        int m_Position;
        float m_StartAngle;

        /**
        * Applies a sinus effect to a text
        *@aram text - text to draw
        *@param width - sinus text target width
        *@param height - sinus text target height
        *@param pFont - font to use to draw text
        *@param bgColor - background color
        *@param pTarget - target component on which sinus text will be drawn
        *@param horzScroll - if true, text will scroll horizontally
        */
        void __fastcall SinusText(const std::wstring& text, int width, int height, TFont* pFont,
                TColor bgColor, TWinControl* pTarget, bool horzScroll);
};
extern PACKAGE TMainForm* MainForm;
#endif
