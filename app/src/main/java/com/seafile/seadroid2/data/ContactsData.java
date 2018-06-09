//package com.seafile.seadroid2.data;
//
//import java.util.ArrayList;
//import java.util.List;
//
///**
// * Function:
// * Author:      Saud
// * Create:      2016/11/12
// * Modtime:     2016/11/12
// */
//public class ContactsData {
//    private String userid;
//    private String name;
//    private List<PhoneInfo> phoneList = new ArrayList<PhoneInfo>(); // PhoneNumber
//    private List<EmailInfo> email = new ArrayList<EmailInfo>(); // Email
//
//
//    public String getUserid() {
//        return userid;
//    }
//
//    public void setUserid(String userid) {
//        this.userid = userid;
//    }
//
//    public String getName() {
//        return name;
//    }
//
//    public void setName(String name) {
//        this.name = name;
//    }
//
//
//    public List<PhoneInfo> getPhoneList() {
//        return phoneList;
//    }
//
//    public void setPhoneList(List<PhoneInfo> phoneList) {
//        this.phoneList = phoneList;
//    }
//
//    public List<EmailInfo> getEmail() {
//        return email;
//    }
//
//    public void setEmail(List<EmailInfo> email) {
//        this.email = email;
//    }
//
//
//    public static class PhoneInfo {
//        private int type;
//        private String number;
//
//        public int getType() {
//            return type;
//        }
//
//        public void setType(int type) {
//            this.type = type;
//        }
//
//        public String getNumber() {
//            return number;
//        }
//
//        public void setNumber(String number) {
//            this.number = number;
//        }
//
//        @Override
//        public String toString() {
//            return "PhoneInfo{" +
//                    "type=" + type +
//                    ", number='" + number + '\'' +
//                    '}';
//        }
//    }
//
//
//    public static class EmailInfo {
//        private int type;
//        private String email;
//
//
//        public String getEmail() {
//            return email;
//        }
//
//        public void setEmail(String email) {
//            this.email = email;
//        }
//
//        public int getType() {
//            return type;
//        }
//
//        public void setType(int type) {
//            this.type = type;
//        }
//
//        @Override
//        public String toString() {
//            return "EmailInfo{" +
//                    "type=" + type +
//                    ", email='" + email + '\'' +
//                    '}';
//        }
//    }
//
//    @Override
//    public String toString() {
//        return "UserData{" +
//                "userid='" + userid + '\'' +
//                ", name='" + name + '\'' +
//                ", phoneList=" + phoneList +
//                ", email=" + email +
//                '}';
//    }
//}
