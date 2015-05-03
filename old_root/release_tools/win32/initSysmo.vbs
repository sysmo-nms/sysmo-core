

Set currentPath = CreateObject("Wscript.Shell")
strPath = WScript.ScriptFullName

Set objFSO = CreateObject("Scripting.FileSystemObject")

Set objFile = objFSO.GetFile(strPath)
strFolder = objFSO.GetParentFolderName(objFile)

Call Recurse(strFolder)

Sub Recurse(strFolderPath)
    Set objFolder = objFSO.GetFolder(strFolderPath)
    Dim objFile
    Dim objSubFolder

    For Each objFile In objFolder.Files
        If (InStr(objFile.Name, ".") > 0) Then
            If (LCase(Mid(objFile.Name, InStrRev(objFile.Name, "."))) = ".bat") Then _
                ReplaceInFile(objFile.Path)
        End If
    
    Next

    For Each objSubFolder In objFolder.SubFolders
        Call Recurse(objSubFolder.Path)
    Next
End Sub

Sub ReplaceInFile(strPath)
    Set objReadFile = objFSO.OpenTextFile(strPath, 1)
    strContents = objReadFile.ReadAll
    objReadFile.Close

    strNewContent = Replace(strContents, "!INSTALL_DIR!", strFolder)
    Set objWriteFile = objFSO.OpenTextFile(strPath, 2)
    objWriteFile.WriteLine strNewContent
    objWriteFile.Close
End Sub
