### OWT Calculation ###

### Apply SAM to Rrs

OWT_pahlevan = function(rrs, reference, id) {


      
      require(data.table)
      require(openxlsx)
      require(hsdar)
      require(dplyr)
      require(data.table)
      
      references.spec = speclib(references.t[1:351,], c(400:750))
      
      #SAM calc
      

      point = data.frame(id = id)
      
      for(i in 1:nrow(rrs)) {
        
        
        
        pt = rrs[i,] %>% dplyr::select(contains(paste('Rrs_', 400:750, sep = ''))) %>% t() %>% data.frame() %>% na.omit()
        
        
        pt = pt[,1]/sum(pt[,1])
        
        if(length(pt) == 351) {
          
          spectra = speclib(pt, c(400:750))
          
          distance <-sam(spectra, 
                         references.spec)  
          
          point[i,2:8] = distance
          
          
        }
        
        if(length(pt) != 351) {
          
          
          point[i,2:8] = NA
          
        }
        
        print(i)
        
      }
      
      
      point$Class = 0
      
      for(i in 1:nrow(point)) {
        
        df = data.frame(Class = 1:7, point = t(point[i,-c(1,9)]))
        names(df)[2] = 'Point'
        res = dplyr::filter(df, Point == min(df$Point))
        
        if(nrow(res) == 0) { 
          
          point$Class[i] = NA
          
        } else{ 
          
          point$Class[i] = res$Class
          
        }
        
        print(i)
        
      }
      
      return(point)

}



