/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                   */
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */
package com.ibm.cicsdev.restappext.bean;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Bean used by the com.ibm.cicsdev.restappext.TaskInformation class to return
 * basic task information in the form of JSON to a requester.
 * 
 * @author Michael Jones
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "status")
public class TaskBean
{
    /**
     * Stores the transaction code
     */
    @XmlElement(name = "transid")
    private String transid;

    /**
     * Stores the user ID associated with task
     */
    @XmlElement(name = "userid")
    private String userid;

    /**
     * Stores the task number for the request
     */
    @XmlElement(name = "tasknum")
    private String tasknum;

    /**
     * Returns the 4 letter transaction code as a String to the caller.
     * 
     * @return String 4 letter transaction code
     */
    public String getTransid() {
        return transid;
    }

    /**
     * Called to set the transaction ID. Doesn't take an input as it relies on
     * the Task object to resolve the id.
     */
    public void setTransid(String tranName) {
        transid = tranName;
    }

    /**
     * Returns a String containing the user ID associated with the current task.
     * 
     * @return String Current user ID
     */
    public String getUserid() {
        return userid;
    }

    /**
     * Called to set the user id field. Does not take a parameter as it relies
     * on the Task class to resolve the actual value.
     */
    public void setUserid(String user) {
        userid = user;
    }

    /**
     * Returns the current task number to the caller as a String.
     * 
     * @return String The current task number
     */
    public String getTasknum() {
        return tasknum;
    }

    /**
     * Called to set the task number. Doesn't take a parameter as we rely on the
     * CICS library to provide the actual value.
     */
    public void setTasknum(String cicsTaskNum) {
        tasknum = cicsTaskNum;
    }
}
