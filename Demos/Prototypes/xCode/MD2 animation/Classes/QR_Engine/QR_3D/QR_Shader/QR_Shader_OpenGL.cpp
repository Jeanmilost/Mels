/*****************************************************************************
 * ==> QR_Shader_OpenGL -----------------------------------------------------*
 *****************************************************************************
 * Description : OpenGL shader language                                      *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_Shader_OpenGL.h"

// std
#include <fstream>
#include <sstream>

// qr engine
#include "QR_Exception.h"

//------------------------------------------------------------------------------
// class QR_Shader_OpenGL - c++ cross platform
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_Shader_OpenGL::QR_Shader_OpenGL() :
        m_ProgramID(0),
        m_VertexID(0),
        m_FragmentID(0),
        m_fOnBindAttribute(NULL)
    {}
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_Shader_OpenGL::~QR_Shader_OpenGL()
    {
        // delete vertex shader, if needed
        if (m_VertexID)
        {
            glDetachShader(m_ProgramID, m_VertexID);
            glDeleteShader(m_VertexID);
            m_VertexID = 0;
        }

        // delete fragment shader, if needed
        if (m_FragmentID)
        {
            glDetachShader(m_ProgramID, m_FragmentID);
            glDeleteShader(m_FragmentID);
            m_FragmentID = 0;
        }

        // delete program, if needed
        if (m_ProgramID)
        {
            glDeleteProgram(m_ProgramID);
            m_ProgramID = 0;
        }
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_Shader_OpenGL::CreateProgram()
    {
        // create new shader program
        m_ProgramID = glCreateProgram();

        // succeeded?
        if (!m_ProgramID)
            M_THROW_EXCEPTION("Failed to create shader program");
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_UIntPtrT QR_Shader_OpenGL::GetProgramID() const
    {
        return m_ProgramID;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_UIntPtrT QR_Shader_OpenGL::AttachFile(const std::string& fileName, IEType type)
    {
        // compile shader
        GLuint shaderID = CompileFile(fileName, ShaderTypeToOpenGLShaderType(type));

        // succeeded?
        if (!shaderID)
            return 0;

        // attach shader to program
        glAttachShader(m_ProgramID, shaderID);

        // do bind attributes?
        if (m_fOnBindAttribute)
            m_fOnBindAttribute(m_ProgramID, type);

        // search for shader type
        switch (type)
        {
            case IE_ST_Vertex:
                m_VertexID = shaderID;
                break;

            case IE_ST_Fragment:
                m_FragmentID = shaderID;
                break;

            default:
                M_THROW_EXCEPTION("Unknown shader type");
        }

        return shaderID;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_UIntPtrT QR_Shader_OpenGL::Attach(const std::string& source, IEType type)
    {
        // compile shader
        GLuint shaderID = Compile(source, ShaderTypeToOpenGLShaderType(type));

        // succeeded?
        if (!shaderID)
            return 0;

        // attach shader to program
        glAttachShader(m_ProgramID, shaderID);

        // do bind attributes?
        if (m_fOnBindAttribute)
            m_fOnBindAttribute(m_ProgramID, type);

        // search for shader type
        switch (type)
        {
            case IE_ST_Vertex:
                m_VertexID = shaderID;
                break;

            case IE_ST_Fragment:
                m_FragmentID = shaderID;
                break;

            default:
                M_THROW_EXCEPTION("Unknown shader type");
        }

        return shaderID;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    bool QR_Shader_OpenGL::Link(bool use)
    {
        // link program
        glLinkProgram(m_ProgramID);

        GLint linked;

        // query program to know if link succeeded
        glGetProgramiv(m_ProgramID, GL_LINK_STATUS, &linked);

        // succeeded?
        if (!linked)
            return false;

        // do use linked program immediately?
        if (use)
            Use(true);

        return true;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_Shader_OpenGL::Use(bool use)
    {
        // do use program and program exists?
        if (use && m_ProgramID)
            // bind program
            glUseProgram(m_ProgramID);
        else
            // unbind program
            glUseProgram(0);
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    GLenum QR_Shader_OpenGL::ShaderTypeToOpenGLShaderType(IEType type)
    {
        switch (type)
        {
            case IE_ST_Vertex:
                return GL_VERTEX_SHADER;

            case IE_ST_Fragment:
                return GL_FRAGMENT_SHADER;

            default:
                M_THROW_EXCEPTION("Unknown shader type");
        }
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_Shader::IEType QR_Shader_OpenGL::OpenGLTypeToShaderType(GLenum type)
    {
        switch (type)
        {
            case GL_VERTEX_SHADER:
                return IE_ST_Vertex;

            case GL_FRAGMENT_SHADER:
                return IE_ST_Fragment;

            default:
                M_THROW_EXCEPTION("Unknown shader type");
        }
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_Shader_OpenGL::Set_OnBindAttribute(ITfOnBindAttribute fHandler)
    {
        m_fOnBindAttribute = fHandler;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    GLuint QR_Shader_OpenGL::CompileFile(const std::string& fileName, GLenum type)
    {
        // open shader file
        std::ifstream shaderFile(fileName.c_str());

        std::ostringstream sstr;
        std::string        line;

        // iterate through file lines
        while (std::getline(shaderFile, line))
            // add line to source buffer to compile
            sstr << line << "\n";

        return Compile(sstr.str(), type);
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    GLuint QR_Shader_OpenGL::Compile(const std::string& source, GLenum type)
    {
        // create new shader
        GLuint shader = glCreateShader(type);

        // get source buffer
        const QR_Char* pSource = source.c_str();

        // compile shader
        glShaderSource(shader, 1, &pSource, NULL);
        glCompileShader(shader);

        // failed?
        if (!shader)
            return 0;

        // return shader index
        return shader;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------

