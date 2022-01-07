#include <Windows.h>
#include "buffer/buffer.h"

typedef VOID(__cdecl *tCreateA)(void *, LPCSTR, LPCSTR, bool);
typedef VOID(__cdecl *tCreateW)(void *, LPCWSTR, LPCWSTR, bool);
typedef VOID(__cdecl *tEnqueueMessage)(void *, void *, HANDLE, const buffer &);
typedef VOID(__cdecl *tReadData)(void *, void *, HANDLE, DWORD, LPCSTR);
typedef VOID(__cdecl *tErrorA)(void *, LPCSTR, HRESULT, LPCSTR, LPCSTR, int);
typedef VOID(__cdecl *tErrorW)(void *, LPCWSTR, HRESULT, LPCWSTR, LPCWSTR, int);
typedef VOID(__cdecl *tDisconnected)(void *, void *, HANDLE);
typedef VOID(__cdecl *tOperationCreateA)(void *, void *, HANDLE, LPCSTR);
typedef VOID(__cdecl *tOperationCreateW)(void *, void *, HANDLE, LPCWSTR);
typedef VOID(__cdecl *tOperationProgress)(void *, void *, HANDLE, int, DWORD);
typedef VOID(__cdecl *tOperationRemove)(void *, void *, HANDLE);
typedef VOID(__cdecl *tPipeCreate)(void *, void *, HANDLE);
typedef VOID(__cdecl *tPipeReadInit)(void *, void *, HANDLE, DWORD);
typedef VOID(__cdecl *tPipeWriteInit)(void *, void *, HANDLE, const buffer &);
typedef VOID(__cdecl *tPipeClose)(void *, void *, HANDLE);

class CCsNamedPipe
{
public:
    CCsNamedPipe() 
		: m_version(0), m_correlationId(0)
    {
	    // Try and find the dll
	    m_hDll = LoadLibraryW(L"CsEtw.dll");
		
	    if (m_hDll)
	    {
		    m_pCreateA = (tCreateA)GetProcAddress(m_hDll, "CsNpCreateA");
		    m_pCreateW = (tCreateW)GetProcAddress(m_hDll, "CsNpCreateW");
		    m_pEnqueueMessage = (tEnqueueMessage)GetProcAddress(m_hDll, "CsNpEnqueueMessage");
		    m_pReadData = (tReadData)GetProcAddress(m_hDll, "CsNpReadData");
		    m_pErrorA = (tErrorA)GetProcAddress(m_hDll, "CsNpErrorA");
		    m_pErrorW = (tErrorW)GetProcAddress(m_hDll, "CsNpErrorW");
		    m_pDisconnected = (tDisconnected)GetProcAddress(m_hDll, "CsNpDisconnected");
		    m_pOperationCreateA = (tOperationCreateA)GetProcAddress(m_hDll, "CsNpOperationCreateA");
		    m_pOperationCreateW = (tOperationCreateW)GetProcAddress(m_hDll, "CsNpOperationCreateW");
		    m_pOperationProgress = (tOperationProgress)GetProcAddress(m_hDll, "CsNpOperationProgress");
		    m_pOperationRemove = (tOperationRemove)GetProcAddress(m_hDll, "CsNpOperationRemove");
		    m_pPipeCreate = (tPipeCreate)GetProcAddress(m_hDll, "CsNpPipeCreate");
		    m_pPipeReadInit = (tPipeReadInit)GetProcAddress(m_hDll, "CsNpPipeReadInit");
		    m_pPipeWriteInit = (tPipeWriteInit)GetProcAddress(m_hDll, "CsNpPipeWriteInit");
		    m_pPipeClose = (tPipeClose)GetProcAddress(m_hDll, "CsNpPipeClose");
	    }
	    else
	    {
		    m_pCreateA = NULL;
		    m_pCreateW = NULL;
		    m_pEnqueueMessage = NULL;
		    m_pReadData = NULL;
		    m_pErrorA = NULL;
		    m_pErrorW = NULL;
		    m_pDisconnected = NULL;
		    m_pOperationCreateA = NULL;
		    m_pOperationCreateW = NULL;
		    m_pOperationProgress = NULL;
		    m_pOperationRemove = NULL;
		    m_pPipeCreate = NULL;
		    m_pPipeReadInit = NULL;
		    m_pPipeWriteInit = NULL;
		    m_pPipeClose = NULL;
	    }
    }

	virtual ~CCsNamedPipe()
    {
		if (m_hDll)
		{
			FreeLibrary(m_hDll);
			m_hDll = NULL;
		}
    }

	HMODULE m_hDll;
	int m_version;
	volatile LONG m_correlationId;

    tCreateA m_pCreateA;
    tCreateW m_pCreateW;
    tEnqueueMessage m_pEnqueueMessage;
    tReadData m_pReadData;
    tErrorA m_pErrorA;
    tErrorW m_pErrorW;
    tDisconnected m_pDisconnected;
    tOperationCreateA m_pOperationCreateA;
    tOperationCreateW m_pOperationCreateW;
    tOperationProgress m_pOperationProgress;
    tOperationRemove m_pOperationRemove;
    tPipeCreate m_pPipeCreate;
    tPipeReadInit m_pPipeReadInit;
    tPipeWriteInit m_pPipeWriteInit;
    tPipeClose m_pPipeClose;
} g_CsNamedPipe;


VOID __cdecl CsNpCreateA(void * channel, LPCSTR hostName, LPCSTR pipeName, bool isServer)
{
    if(g_CsNamedPipe.m_pCreateA)
        g_CsNamedPipe.m_pCreateA(channel, hostName, pipeName, isServer);
}

VOID __cdecl CsNpCreateW(void * channel, LPCWSTR hostName, LPCWSTR pipeName, bool isServer)
{
    if(g_CsNamedPipe.m_pCreateW)
        g_CsNamedPipe.m_pCreateW(channel, hostName, pipeName, isServer);
}

VOID __cdecl CsNpEnqueueMessage(void * channel, void * operation, HANDLE pipeHandle, const buffer & buf)
{
    if(g_CsNamedPipe.m_pEnqueueMessage)
        g_CsNamedPipe.m_pEnqueueMessage(channel, operation, pipeHandle, buf);
}

VOID __cdecl CsNpReadData(void * channel, void * operation, HANDLE pipeHandle, DWORD length, LPCSTR data)
{
    if(g_CsNamedPipe.m_pReadData)
        g_CsNamedPipe.m_pReadData(channel, operation, pipeHandle, length, data);
}

VOID __cdecl CsNpErrorA(void * channel, LPCSTR message, HRESULT errorCode, LPCSTR errorText, LPCSTR file, int line)
{
    if(g_CsNamedPipe.m_pErrorA)
        g_CsNamedPipe.m_pErrorA(channel, message, errorCode, errorText, file, line);
}

VOID __cdecl CsNpErrorW(void * channel, LPCWSTR message, HRESULT errorCode, LPCWSTR errorText, LPCWSTR file, int line)
{
    if(g_CsNamedPipe.m_pErrorW)
        g_CsNamedPipe.m_pErrorW(channel, message, errorCode, errorText, file, line);
}

VOID __cdecl CsNpDisconnected(void * channel, void * operation, HANDLE pipeHandle)
{
    if(g_CsNamedPipe.m_pDisconnected)
        g_CsNamedPipe.m_pDisconnected(channel, operation, pipeHandle);
}

VOID __cdecl CsNpOperationCreateA(void * channel, void * operation, HANDLE pipeHandle, LPCSTR type)
{
    if(g_CsNamedPipe.m_pOperationCreateA)
        g_CsNamedPipe.m_pOperationCreateA(channel, operation, pipeHandle, type);
}

VOID __cdecl CsNpOperationCreateW(void * channel, void * operation, HANDLE pipeHandle, LPCWSTR type)
{
    if(g_CsNamedPipe.m_pOperationCreateW)
        g_CsNamedPipe.m_pOperationCreateW(channel, operation, pipeHandle, type);
}

VOID __cdecl CsNpOperationProgress(void * channel, void * operation, HANDLE pipeHandle, int state, DWORD bytesTransferred)
{
    if(g_CsNamedPipe.m_pOperationProgress)
        g_CsNamedPipe.m_pOperationProgress(channel, operation, pipeHandle, state, bytesTransferred);
}

VOID __cdecl CsNpOperationRemove(void * channel, void * operation, HANDLE pipeHandle)
{
    if(g_CsNamedPipe.m_pOperationRemove)
        g_CsNamedPipe.m_pOperationRemove(channel, operation, pipeHandle);
}

VOID __cdecl CsNpPipeCreate(void * channel, void * operation, HANDLE pipeHandle)
{
    if(g_CsNamedPipe.m_pPipeCreate)
        g_CsNamedPipe.m_pPipeCreate(channel, operation, pipeHandle);
}

VOID __cdecl CsNpPipeReadInit(void * channel, void * operation, HANDLE pipeHandle, DWORD expected)
{
    if(g_CsNamedPipe.m_pPipeReadInit)
        g_CsNamedPipe.m_pPipeReadInit(channel, operation, pipeHandle, expected);
}

VOID __cdecl CsNpPipeWriteInit(void * channel, void * operation, HANDLE pipeHandle, const buffer & buf)
{
    if(g_CsNamedPipe.m_pPipeWriteInit)
        g_CsNamedPipe.m_pPipeWriteInit(channel, operation, pipeHandle, buf);
}

VOID __cdecl CsNpPipeClose(void * channel, void * operation, HANDLE pipeHandle)
{
    if(g_CsNamedPipe.m_pPipeClose)
        g_CsNamedPipe.m_pPipeClose(channel, operation, pipeHandle);
}