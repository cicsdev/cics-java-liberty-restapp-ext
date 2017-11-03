/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2016 All Rights Reserved                   */       
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */
package com.ibm.cicsdev.restappext;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.ibm.cicsdev.restappext.bean.CICSInfoBean;

/**
 * Simple service that provides some basic task information from
 * CICS when requested via HTTP GET.
 * 
 * @author Michael Jones (michaej8@uk.ibm.com)
 *
 */
@Path("taskInformation")
public class TaskInformation {

	@GET
        @Produces(MediaType.APPLICATION_JSON)
	public CICSInfoBean getTaskInfo(){
		CICSInfoBean cicsInfo = new CICSInfoBean();
		return cicsInfo;
	}
}