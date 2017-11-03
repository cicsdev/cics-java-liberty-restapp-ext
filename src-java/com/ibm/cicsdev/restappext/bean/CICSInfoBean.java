/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2016 All Rights Reserved                   */       
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */
package com.ibm.cicsdev.restappext.bean;

import com.ibm.cics.server.CicsException;
import com.ibm.cics.server.Task;

/**
 * Bean used by the com.ibm.cicsdev.restappext.TaskInformation
 * class to return basic task information in the form of JSON
 * to a requester.
 * 
 * @author Michael Jones (michaej8@uk.ibm.com)
 *
 */
public class CICSInfoBean {
	
	/**
	 * Sets up all the required fields for the class
	 * as part of the construction method
	 */
	public CICSInfoBean(){
		this.setTasknum();
		this.setTransid();
		this.setUserid();
	}

	/** Stores the transaction code **/
	public String transid;
	
	/** Stores the user ID associated with task **/
	public String userid;
	
	/** Stores the task number for the request **/
	public String tasknum;
	
	/** Used by the class to avoid re-initializing the task
	 * object with each call to the set methods.
	 */
	private Task task;
	
	/**
	 * Returns the 4 letter transaction code as a
	 * String to the caller.
	 * 
	 * @return String 4 letter transaction code
	 */
	public String getTransid() {
		return transid;
	}
	
	/**
	 * Called to set the transaction ID. Doesn't take
	 * an input as it relies on the Task object to
	 * resolve the id.
	 */
	public void setTransid(){
		
		if(task == null){
			task = Task.getTask();
		}
		
		transid = task.getTransactionName();
		
	}
	
	/**
	 * Returns a String containing the user ID
	 * associated with the current task.
	 * 
	 * @return String Current user ID
	 */
	public String getUserid() {
		return userid;
	}
	
	/**
	 * Called to set the user id field. Does not take
	 * a parameter as it relies on the Task class
	 * to resolve the actual value.
	 */
	public void setUserid(){
		
		if(task == null){
			task = Task.getTask();
		}
		
		try {
			userid = task.getUSERID();
		} catch(CicsException e){
			e.printStackTrace();
			userid = "CICS_ERROR_SEE_LOG";
		}
	}
	
	/**
	 * Returns the current task number to the caller
	 * as a String.
	 * 
	 * @return String The current task number
	 */
	public String getTasknum() {
		System.out.println("Current task number: " + tasknum );
		return tasknum;
	}
	
	/**
	 * Called to set the task number. Doesn't take a parameter
	 * as we rely on the CICS library to provide the actual 
	 * value.
	 */
	public void setTasknum(){
		
		if(task == null){
			task = Task.getTask();
		}
		
		tasknum = Integer.toString(task.getTaskNumber());
		
	}
	
}
