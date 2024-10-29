package com.seafile.seadroid2.framework.data.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

public class SDocPageOptionsModel implements Parcelable {
    public String docName;
    public String docUuid;
    public String seadocServerUrl;
    public String seadocAccessToken;
    public String repoID;
    public String repoName;
    public boolean isLocked;
    public boolean isStarred;

    @Override
    public String toString() {
        return "SDocPageOptionsModel{" +
                "docName='" + docName + '\'' +
                ", docUuid='" + docUuid + '\'' +
                ", seadocServerUrl='" + seadocServerUrl + '\'' +
                ", seadocAccessToken='" + seadocAccessToken + '\'' +
                ", repoID='" + repoID + '\'' +
                ", repoName='" + repoName + '\'' +
                ", isLocked=" + isLocked +
                ", isStarred=" + isStarred +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.docName);
        dest.writeString(this.docUuid);
        dest.writeString(this.seadocServerUrl);
        dest.writeString(this.seadocAccessToken);
        dest.writeString(this.repoID);
        dest.writeString(this.repoName);
        dest.writeByte(this.isLocked ? (byte) 1 : (byte) 0);
        dest.writeByte(this.isStarred ? (byte) 1 : (byte) 0);
    }

    public SDocPageOptionsModel() {
    }

    protected SDocPageOptionsModel(Parcel in) {
        this.docName = in.readString();
        this.docUuid = in.readString();
        this.seadocServerUrl = in.readString();
        this.seadocAccessToken = in.readString();
        this.repoID = in.readString();
        this.repoName = in.readString();
        this.isLocked = in.readByte() != 0;
        this.isStarred = in.readByte() != 0;
    }

    public static final Creator<SDocPageOptionsModel> CREATOR = new Creator<SDocPageOptionsModel>() {
        @Override
        public SDocPageOptionsModel createFromParcel(Parcel source) {
            return new SDocPageOptionsModel(source);
        }

        @Override
        public SDocPageOptionsModel[] newArray(int size) {
            return new SDocPageOptionsModel[size];
        }
    };
}

//<script type="text/javascript">
//window.app = {
//config: {
//mediaUrl: '/media/',
//logoPath: 'img/seafile-logo.png',
//logoWidth: '',
//logoHeight: '32',
//faviconPath: 'favicons/favicon.png',
//loginBGPath: 'img/login-bg.jpg',
//siteTitle: 'Seafile Cloud',
//siteName: 'Seafile Cloud',
//siteRoot: '/',
//loginUrl: '/accounts/login/',
//isPro: 'True',
//isDBSqlite3:  false ,
//isDocs: 'False',
//lang: 'zh-cn',
//fileServerRoot: 'https://cloud.seafile.com/seafhttp/',
//enableRepoAutoDel:  false ,
//useGoFileserver:  true ,
//serviceURL: 'https://cloud.seafile.com',
//seafileVersion: '11.0.10',
//avatarURL: 'https://cloud.seafile.com/media/avatars/3/b/951e8ce3d2a07ae95b417f475d9222/resized/72/1b041380545f747477abcd9de3d58fae.png'
//        },
//pageOptions: {
//csrfToken: "90ADuJdkx32qYA5uwgY3tMIgCMBJweUtWRDrWMPt9u5NFRAC51YtOTMouhBkQaZm",
//seafileCollabServer: '',
//name: "大辉",
//contactEmail: "zhwanng@163.com",
//username: "f4f550ea33e14f82aab7da71be0d13fa@auth.local",
//guideEnabled:  false ,
//trashReposExpireDays:  null ,
//canAddRepo:  true ,
//canShareRepo:  true ,
//canAddGroup:  true ,
//groupImportMembersExtraMsg: "",
//canGenerateShareLink:  false ,
//canGenerateUploadLink:  true ,
//canSendShareLinkEmail:  true ,
//canViewOrg:'False',
//fileAuditEnabled:  false ,
//enableFileComment:  false ,
//folderPermEnabled:  false ,
//enableResetEncryptedRepoPassword: '',
//isEmailConfigured: '',
//enableUploadFolder: 'True',
//enableResumableFileUpload: 'False',
//resumableUploadFileBlockSize: '',
//// storage backends
//storages: (function () {
//    var storages = [];
//
//    return storages;
//})(),
//// library template
//libraryTemplates: (function () {
//    var libraryTemplates = [];
//
//    return libraryTemplates;
//})(),
//enableRepoSnapshotLabel:  false ,
//shareLinkForceUsePassword:  false ,
//shareLinkPasswordMinLength: 10,
//shareLinkPasswordStrengthLevel: 1,
//sideNavFooterCustomHtml: "",
//aboutDialogCustomHtml: "",
//maxFileName: "255",
//canPublishRepo:  true ,
//enableEncryptedLibrary:  false ,
//enableRepoHistorySetting:  false ,
//isSystemStaff:  false ,
//thumbnailSizeForOriginal: 1024,
//repoPasswordMinLength: 8,
//canAddPublicRepo:  false ,
//enableOCMViaWebdav:  false ,
//enableOCM:  false ,
//ocmRemoteServers: (function () {
//    var servers = [];
//
//    return servers;
//})(),
//canInvitePeople:  false ,
//customNavItems:  [] ,
//enableShowContactEmailWhenSearchUser:  false ,
//
//
//
//enableTC:  false ,
//enableSSOToThirdpartWebsite:  false ,
//enableVideoThumbnail:  false ,
//showLogoutIcon:  false ,
//additionalShareDialogNote:  null ,
//additionalAppBottomLinks:  null ,
//additionalAboutDialogLinks:  null ,
//enableOnlyoffice:  true ,
//onlyofficeConverterExtensions:  ['.docm', '.doc', '.dotx', '.dotm', '.dot', '.odt', '.fodt', '.ott', '.xlsm', '.xls', '.xltx', '.xltm', '.xlt', '.ods', '.fods', '.ots', '.pptm', '.ppt', '.ppsx', '.ppsm', '.pps', '.potx', '.potm', '.pot', '.odp', '.fodp', '.otp', '.rtf', '.mht', '.html', '.htm', '.xml', '.epub', '.fb2'] ,
//enableSeadoc:  true ,
//enableSeafileAI:  false ,
//canSetExProps:  false ,
//enableSeaTableIntegration:  false ,
//isOrgContext:  false ,
//        }
//        };
//</script>
//<script type="text/javascript">
//// overwrite the one in base_for_react.html
//window.app.pageOptions = {
//server: 'https://cloud.seafile.com',
//username: 'f4f550ea33e14f82aab7da71be0d13fa@auth.local',
//userNickName: '大辉',
//
//canGenerateShareLink:  false ,
//canSendShareLinkEmail:  true ,
//shareLinkForceUsePassword:  false ,
//shareLinkPasswordMinLength: 10,
//shareLinkPasswordStrengthLevel: 1,
//shareLinkExpireDaysDefault: 0,
//shareLinkExpireDaysMin: 0,
//shareLinkExpireDaysMax: 0,
//
//// for all types of files
//fileName: 'sssssssssss.sdoc',
//isStarred: false,
//isLocked: false,
//latestContributor: 'f4f550ea33e14f82aab7da71be0d13fa@auth.local',
//latestContributorName: '大辉',
//lastModificationTime: '1724914095',
//repoID: 'a62feaaa-191a-4b20-b0bf-6566a5c831ff',
//repoName: '冲冲冲冲冲冲冲',
//repoEncrypted: false,
//filePath: '/sssssssssss.sdoc',
//filePerm: 'rw',
//fileType: 'SDoc',
//parentDir: '/',
//err: '',
//seafileCollabServer: '',
//contactEmail: "zhwanng@163.com",
//lockedByMe: false,
//canLockUnlockFile: true,
//canEditFile: true, // only for some file types
//canDownloadFile: true,
//enableWatermark: false,
//
//// for SDoc file
//
//isPro: 'True',
//docPath: '/sssssssssss.sdoc',
//docName: 'sssssssssss.sdoc',
//docUuid: '811898d9-7328-4f2b-9b0a-01c8fe51c943',
//assetsUrl: '/api/v2.1/seadoc/download-image/811898d9-7328-4f2b-9b0a-01c8fe51c943',
//seadocAccessToken: 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaWxlX3V1aWQiOiI4MTE4OThkOS03MzI4LTRmMmItOWIwYS0wMWM4ZmU1MWM5NDMiLCJmaWxlbmFtZSI6InNzc3Nzc3Nzc3NzLnNkb2MiLCJ1c2VybmFtZSI6ImY0ZjU1MGVhMzNlMTRmODJhYWI3ZGE3MWJlMGQxM2ZhQGF1dGgubG9jYWwiLCJuYW1lIjoiXHU1OTI3XHU4Zjg5IiwiYXZhdGFyX3VybCI6Imh0dHBzOi8vY2xvdWQuc2VhZmlsZS5jb20vbWVkaWEvYXZhdGFycy8zL2IvOTUxZThjZTNkMmEwN2FlOTViNDE3ZjQ3NWQ5MjIyL3Jlc2l6ZWQvNzIvMWIwNDEzODA1NDVmNzQ3NDc3YWJjZDlkZTNkNThmYWUucG5nIiwicGVybWlzc2lvbiI6InJ3IiwiZXhwIjoxNzMwMTczNTcwfQ.isWMkkrpVMHa_E4Urd4jlWpuYIH1ytJ46okze1_9mkA',
//seadocServerUrl: 'https://cloud.seafile.com/sdoc-server',
//isSdocRevision: false,
//revisionId: '',
//originDocUuid: '',
//originParentPath: '',
//originFilename: '',
//originFilePath: '',
//originFileVersion: '',
//publishFileVersion: '',
//publisher: '',
//publisherNickname: '',
//isPublished: false,
//revisionCreatedAt: '',
//revisionUpdatedAt: '',
//isSdocDraft: false,
//isFreezed: false
//
//        };
//window.app.userInfo = {
//username: 'f4f550ea33e14f82aab7da71be0d13fa@auth.local',
//name: '大辉',
//contact_email: 'zhwanng@163.com',
//        }
//</script>