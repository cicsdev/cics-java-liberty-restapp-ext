/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */

package com.ibm.cicsdev.restappext.bean;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.ibm.cicsdev.restappext.generated.StockPart;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "stockPartCollection")
public class StockPartCollection
{
    @XmlElement(name = "resourceName")
    private String resourceName;

    @XmlElement(name = "stockPartList")
    private List<StockPartBean> stockPartList = new ArrayList<>();
    
    public String getResourceName()
    {
        return resourceName;
    }

    public void setResourceName(String resourceName)
    {
        this.resourceName = resourceName;
    }
    
    public void add(StockPart sp)
    {
        // Create a corresponding StockPartBean object and add to our list
        this.stockPartList.add(new StockPartBean(sp));
    }

}
