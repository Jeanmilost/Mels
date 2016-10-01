/******************************************************************************
 * ==> Main ------------------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 normals table file generator main interface              *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <memory>

// interface
#include "QR_FileTools.h"
#include "QR_MD2Normals.h"
#include "QR_MD2NormalsFile.h"

#pragma package(smart_init)
#pragma resource "*.dfm"

//------------------------------------------------------------------------------
// Specific VCL macros
//------------------------------------------------------------------------------
#define M_UniToChar(str)  (str.IsEmpty() ? ""  : AnsiString(str).c_str())
#define M_UniToWChar(str) (str.IsEmpty() ? L"" : str.c_str())
#define M_StrToUni(str)   (str.empty()   ? ""  : UnicodeString(AnsiString(str.c_str()))
#define M_WStrToUni(str)  (str.empty()   ? L"" : UnicodeString(str.c_str())
//---------------------------------------------------------------------------
TMainForm* MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_StartAngle(0.0f),
    m_Position(0)
{}
//---------------------------------------------------------------------------
void __fastcall TMainForm::tiAnimateTimer(TObject* pSender)
{
    // get title text
    std::wstring text = M_UniToWChar(paTitle->Caption);

    // show title as sinus text
    SinusText(text, paTitle->Width, paTitle->Height, paTitle->Font, Color, paTitle, false);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::btBrowseClick(TObject* pSender)
{
    // show save dialog box, and check if used canceled selection
    if (!sdSaveDialog->Execute())
    {
        meLog->Lines->Add(L"File selection canceled by user.");
        return;
    }

    std::string fileName = M_UniToChar(sdSaveDialog->FileName);

    if (QR_FileTools::FileExists(fileName))
    {
        meLog->Lines->Add(L"File " + sdSaveDialog->FileName + " already exists on the selected location.");

        // failed
        if (::MessageBox(Handle,
                         L"File already exists on the selected location.\r\n\r\nDo you want to continue?",
                         L"File already exists",
                         MB_ICONWARNING | MB_OKCANCEL) != IDOK)
        {
            meLog->Lines->Add(L"File selection canceled by user.");
            return;
        }
    }

    // file selected, show it
    edFileName->Text = sdSaveDialog->FileName;

    meLog->Lines->Add(L"File " + sdSaveDialog->FileName + L" selected.");
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::btGenerateClick(TObject* pSender)
{
    meLog->Lines->Add(L"Generate MD2 normals table file...");

    // get file name to write
    std::string fileName = M_UniToChar(edFileName->Text);

    // is file name empty?
    if (fileName.empty())
    {
        // failed
        ::MessageBox(Handle, L"File name is empty.", L"File name is empty", MB_ICONERROR);
        meLog->Lines->Add(L"FAILED - file name is empty.");
        return;
    }

    QR_MD2NormalsFile file(1.0f, M_MD2_NormalCount);

    // save MD2 normals file
    if (!file.Save(fileName, meLog))
    {
        // failed
        ::MessageBoxA(Handle, ("Failed to save file:\r\n" + fileName).c_str(), "Failed to save file",
                MB_ICONERROR);
        return;
    }

    // succeeded
    ::MessageBoxA(Handle, ("The file:\r\n" + fileName + "\r\nwas saved successfully.").c_str(),
            "File saved", MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SinusText(const std::wstring& text, int width, int height, TFont* pFont,
        TColor bgColor, TWinControl* pTarget, bool horzScroll)
{
    // no text?
    if (text.empty())
        return;

    // no target?
    if (!pTarget)
        return;

    // no font?
    if (!pFont)
        return;

    const unsigned extraHeight = 10;

    // get target device context
    HDC hTargetDC = ::GetDC(pTarget->Handle);

    // found it?
    if (!hTargetDC)
        return;

    try
    {
        // set font to use
        ::SelectObject(hTargetDC, pFont->Handle);
        ::SetTextColor(hTargetDC, pFont->Color);

        TRect bufferRect;

        // measure text to draw
        ::DrawTextW(hTargetDC, text.c_str(), text.length(), &bufferRect, DT_LEFT | DT_TOP
                | DT_SINGLELINE | DT_CALCRECT);

        // create source offscreen surface
        std::auto_ptr<TBitmap> pSrcOffscreen(new TBitmap());
        pSrcOffscreen->PixelFormat = pf24bit;
        pSrcOffscreen->Width  = bufferRect.Width();
        pSrcOffscreen->Height = bufferRect.Height() + extraHeight;

        // configure source canvas
        pSrcOffscreen->Canvas->Brush->Style = bsSolid;
        pSrcOffscreen->Canvas->Brush->Color = bgColor;
        pSrcOffscreen->Canvas->Pen->Color   = pFont->Color;
        pSrcOffscreen->Canvas->FillRect(TRect(0, 0, pSrcOffscreen->Width, pSrcOffscreen->Height));

        // set font to use
        pSrcOffscreen->Canvas->Font->Assign(pFont);

        // draw text in source offscreen bitmap
        ::DrawTextW(pSrcOffscreen->Canvas->Handle, text.c_str(), text.length(), &bufferRect, DT_LEFT
                | DT_TOP | DT_SINGLELINE);

        // create destination offscreen surface
        std::auto_ptr<TBitmap> pDstOffscreen(new TBitmap());
        pDstOffscreen->PixelFormat = pf24bit;
        pDstOffscreen->Width       = width;
        pDstOffscreen->Height      = height;

        // configure destination canvas
        pDstOffscreen->Canvas->Brush->Style = bsSolid;
        pDstOffscreen->Canvas->Brush->Color = bgColor;
        pDstOffscreen->Canvas->Pen->Color   = pFont->Color;
        pDstOffscreen->Canvas->FillRect(TRect(0, 0, pDstOffscreen->Width, pDstOffscreen->Height));

        const int   yPos      = (height >> 1) - (bufferRect.Height() >> 1);
              float angle     = m_StartAngle;
        const int   offset    = 10;
        const int   scrollMax = bufferRect.Width();

        // iterate through text area to draw
        for (int i = 0; i < width; i += offset)
        {
            // calculate next area
            const int srcStartPosX = (m_Position + i) % scrollMax;
            const int srcEndPosX   = srcStartPosX + offset;
            const int dstStartPosX = i;
                  int dstEndPosX   = i + offset;
            const int scrollY      = yPos + (20.0f * std::sinf(angle));
            const int scrollHeight = pSrcOffscreen->Height;

            if (dstEndPosX > width)
                dstEndPosX = width;

            // is area out of bounds?
            if (srcEndPosX > scrollMax)
            {
                // scroll areas
                const int intermediateWidth = scrollMax - srcStartPosX;

                ::BitBlt(pDstOffscreen->Canvas->Handle, dstStartPosX, scrollY, intermediateWidth,
                        scrollHeight, pSrcOffscreen->Canvas->Handle, srcStartPosX, 0, SRCCOPY);

                ::BitBlt(pDstOffscreen->Canvas->Handle, dstStartPosX + intermediateWidth, scrollY,
                        srcEndPosX - scrollMax, scrollHeight,pSrcOffscreen->Canvas->Handle, 0, 0,
                        SRCCOPY);
            }
            else
                // draw next area
                ::BitBlt(pDstOffscreen->Canvas->Handle, dstStartPosX, scrollY, dstEndPosX, scrollHeight,
                        pSrcOffscreen->Canvas->Handle, srcStartPosX, 0, SRCCOPY);

            angle -= 0.08f;

            if (angle >= 6.28f)
                angle -= 6.28f;
        }

        m_StartAngle += 0.05f;

        if (m_StartAngle >= 6.28f)
            m_StartAngle -= 6.28f;

        if (horzScroll)
            ++m_Position;

        if (m_Position >= pSrcOffscreen->Width)
            m_Position -= pSrcOffscreen->Width;

        // copy text to target canvas
        ::BitBlt(hTargetDC, 0, 0, width, height, pDstOffscreen->Canvas->Handle, 0, 0, SRCCOPY);
    }
    __finally
    {
        if (hTargetDC)
            ::ReleaseDC(pTarget->Handle, hTargetDC);
    }
}
//---------------------------------------------------------------------------
