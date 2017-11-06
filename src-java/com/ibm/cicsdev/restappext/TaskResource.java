/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                   */       
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */
package com.ibm.cicsdev.restappext;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.server.CicsException;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.restappext.bean.TaskBean;

/**
 * Simple service that provides some basic task information from
 * CICS when requested via HTTP GET.
 * 
 * @author Michael Jones
 *
 */
@Path("taskInformation")
@Produces(MediaType.APPLICATION_JSON)
public class TaskResource
{
    /**
     * Creates a TaskBean and populates it with some
     * basic information about the associated CICS
     * TASK before returning it to the user.
     * 
     * The end result is transferred to JSON.
     * 
     * @return - taskInfo bean with populated TASK data
     */
    @GET
    public TaskBean getTaskInfo() {
        
        // Get hold of our CICS task object and create
        // an empty TaskBean for storing it's information
        Task task = Task.getTask();
        TaskBean cicsInfo = new TaskBean();
        
        // If we didn't get a Task, end the application here
        // with some information indicating the problem
        if (task == null) {
            cicsInfo.setTasknum("NO_CICS_TASK_FOUND");
            cicsInfo.setUserid("NO_CICS_TASK_FOUND");
            cicsInfo.setTransid("NO_CICS_TASK_FOUND");
            return cicsInfo;
        }
        
        // Get the CICS user ID, attempting to handle any
        // exception that occurs as a result
        try {
            cicsInfo.setUserid(task.getUSERID());
        }
        catch (CicsException e) {

            // A CICS failure - build an error message
            String err = String.format("Error obtaining userid : %s", e.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(e, r);
        }
        
        // Retrieve the transaction code and task number
        cicsInfo.setTransid(task.getTransactionName());
        cicsInfo.setTasknum(Integer.toString(task.getTaskNumber()));
        
        return cicsInfo;
    }
}
