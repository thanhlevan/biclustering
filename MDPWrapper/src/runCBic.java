import cs.kuleuven.cp.biclusteringMDL;
import cs.kuleuven.cp.CBic;

import org.apache.commons.cli.*;
import java.io.File;

/**
 * Created on Jan 23rd, 2014
 * 
 *
 */
public class runCBic{
	
	
	public static void main(String[] args) {
		
		Option opBinFile = OptionBuilder.withArgName("BinaryFile").hasArg().withDescription("binary (duplicated) file name").create("bf");
        Option opMulFile = OptionBuilder.withArgName("MulFile").hasArg().withDescription("multi-valued file name").create("mf");
        Option opQueryFile = OptionBuilder.withArgName("QueryFile").hasArg().withDescription("query file name").create("qf");
        Option opRowThreshold = OptionBuilder.withArgName("RowThreshold").hasArg().withDescription("row threshold").create("rth");
        Option opColThreshold = OptionBuilder.withArgName("ColThreshold").hasArg().withDescription("row threshold").create("cth");
        Option opRowThreshold2 = OptionBuilder.withArgName("MDLRowThreshold").hasArg().withDescription("row threshold used to construct potential regions. Should be low").create("rth2");
        Option opColThreshold2 = OptionBuilder.withArgName("MDLColThreshold").hasArg().withDescription("col threshold used to construct potential regions. Should be low").create("cth2");
        Option opFailureThreshold = OptionBuilder.withArgName("FailureThreshold").hasArg().withDescription("failure threshold").create("fth");
        Option opRestartThreshold = OptionBuilder.withArgName("RestartThreshold").hasArg().withDescription("restart threshold").create("sth");
        Option opRemovedRowsFile = OptionBuilder.withArgName("RemovedRowsFile").hasArg().withDescription("File contain rows need to be removed").create("rf");
        Option opWorkingDir = OptionBuilder.withArgName("WorkingDirectory").hasArg().withDescription("Working directory to store results").create("wd");
                
        Option help	= new Option("help", "Print help");
        Option opAuto = new Option("auto", "Run with varying noise levels for mining bi-clusters in potential regions. Noise levels required for constructing potential regions are defined by the user.");
        
        Options options = new Options();
        options.addOption(opBinFile);
        options.addOption(opMulFile);
        options.addOption(opQueryFile);
        options.addOption(opRowThreshold);
        options.addOption(opColThreshold);
        options.addOption(opRowThreshold2);
        options.addOption(opColThreshold2);
        options.addOption(opFailureThreshold);
        options.addOption(opRestartThreshold);
        options.addOption(opRemovedRowsFile);
        options.addOption(opWorkingDir);
                
        options.addOption(help);
        options.addOption(opAuto);
        
        // default values
                
        String binFileName 		= "/home/thanh/data/biclusters/8modules/data/noise_10/dup_bg_0.10_bic_0.10_8modules.txt" ;
        String mulFileName 		= "/home/thanh/data/biclusters/8modules/data/noise_10/bg_0.10_bic_0.10_8modules.txt" ;
        String queryFileName 	= "/home/thanh/querytest.txt" ;
        String rmRowsFileName 	= "" ;
        String workingDir 		= "/home/thanh/test/syn/query_450/" ;
                
        int colThreshold = 25;
        int rowThreshold = 25;
        int mdlColThreshold = 25;
        int mdlRowThreshold = 25;
        int failureThreshold = 500;
        int restartThreshold = 50;
        
        boolean bAuto = false;
                        
        // parse command line to get values
        CommandLineParser parser = new BasicParser();
        
        try{
        	CommandLine cmd = parser.parse( options, args);
        	if (cmd.hasOption("bf")) {
        	    binFileName = cmd.getOptionValue("bf");
        	}        	
        	
        	if (cmd.hasOption("mf")) {
        		mulFileName = cmd.getOptionValue("mf");
        	}
        	
        	if (cmd.hasOption("qf")) {
        		queryFileName = cmd.getOptionValue("qf");
        	}
        	
        	if (cmd.hasOption("wd")) {
        		workingDir = cmd.getOptionValue("wd");
        	}
        	
        	if (cmd.hasOption("rf")) {
        		rmRowsFileName = cmd.getOptionValue("rf");
        	}
        	
        	if (cmd.hasOption("cth")) {
        		colThreshold = Integer.parseInt(cmd.getOptionValue("cth"));
        	}
        	
        	if (cmd.hasOption("rth")) {
        		rowThreshold = Integer.parseInt(cmd.getOptionValue("rth"));
        	}
        	
        	if (cmd.hasOption("cth2")) {
        		mdlColThreshold = Integer.parseInt(cmd.getOptionValue("cth2"));
        	}
        	
        	if (cmd.hasOption("rth2")) {
        		mdlRowThreshold = Integer.parseInt(cmd.getOptionValue("rth2"));
        	}
        	
        	if (cmd.hasOption("fth")) {
        		failureThreshold = Integer.parseInt(cmd.getOptionValue("fth"));
        	}
        	
        	if (cmd.hasOption("sth")) {
        		restartThreshold = Integer.parseInt(cmd.getOptionValue("sth"));
        	}
        	
        	if (cmd.hasOption("help")) {
        		HelpFormatter formatter = new HelpFormatter();
        		formatter.printHelp( "runBiclusteringMDL", options );
        		return;
        	}
        	
        	if (cmd.hasOption("auto")) {
        		bAuto = true;
        	}
        	
        }catch (ParseException ex){
        	System.out.println( ex.getMessage());
        }
        
        System.out.println("Binary File = " + binFileName);
        System.out.println("Multivalued File = " + mulFileName);
        System.out.println("Query File = " + queryFileName);
        System.out.println("Removed rows File = " + rmRowsFileName);
    
        System.out.print("Row noise threshold = "); System.out.println(rowThreshold);
        System.out.print("Col noise threshold = "); System.out.println(colThreshold);
        System.out.print("MDL row noise threshold = "); System.out.println(mdlRowThreshold);
        System.out.print("MDL col noise threshold = "); System.out.println(mdlColThreshold);
        System.out.print("Failure threshold = "); System.out.println(failureThreshold);
        System.out.print("Restart threshold = "); System.out.println(restartThreshold);
        System.out.print("Working directory = "); System.out.println(workingDir);
        
        if (binFileName.isEmpty()) {
        	System.out.println("Binary file is not provided. Stop.");
        	return;
        }
        
        if (mulFileName.isEmpty()) {
        	System.out.println("Multivalued file is not provided. Stop. ");
        	return;
        }
        
        if (queryFileName.isEmpty()) {
        	System.out.println("Query file is not provided. Stop.");
        	return;
        }
        
        if (!bAuto) {        
	        CBic cbic = new CBic(binFileName,
								mulFileName,
								queryFileName,
								workingDir,
								rowThreshold,
								colThreshold,
								mdlRowThreshold,
								mdlColThreshold,
								failureThreshold,
								restartThreshold);							
	        
	        cbic.mineMultiBiclusters();
	        
        } else {
        	
        	runWithVariedNoiseLevels(binFileName,
								mulFileName,
								queryFileName,
								workingDir,								
								mdlRowThreshold,								
								failureThreshold,
								restartThreshold);
        }
	}
	
	public static void runWithVariedNoiseLevels(String binFileName,
			String mulFileName,
			String queryFileName,
			String workingDir,
			int mdlRowColLevels,
			int failureThreshold,
			int restartThreshold) {
		
				
		String[] rowThresholds = {"20", "25", "30"};
        String[] colThresholds = {"20", "25", "30"};
        
        //Number of running repeats for the same combinations of parameters.
        int nExperimentRepeat = 2;
        
        StringBuilder pathBuilder = new StringBuilder();
        pathBuilder.append(workingDir);
        pathBuilder.append("NoiseUsedInPR_");
        pathBuilder.append(Integer.toString(mdlRowColLevels));
        pathBuilder.append("/");
        
        String strMdlNoiseFolder = pathBuilder.toString();
        File mdlNoiseFolder = new File(strMdlNoiseFolder);
        System.out.println("mdlNoiseFolder:" + strMdlNoiseFolder);
        if(!mdlNoiseFolder.exists()){
            mdlNoiseFolder.mkdir();
        }else{
            mdlNoiseFolder.delete();
            mdlNoiseFolder.mkdir();
        }
        
        for (int i = 0; i < rowThresholds.length; i++) {
        	for (int j = 0; j < colThresholds.length; j++) {
        		
        		int rowThreshold = Integer.parseInt(rowThresholds[i]);
        		int colThreshold = Integer.parseInt(colThresholds[j]);
        		
        		StringBuilder b1 = new StringBuilder();
        		b1.append(strMdlNoiseFolder);
        		b1.append("r" + rowThresholds[i] + "_" + "c" + colThresholds[j]);
        		b1.append("/");
        		
        		String strBicNoiseFolder = b1.toString();
        		File bicNoiseFolder = new File(strBicNoiseFolder);
        		if (!bicNoiseFolder.exists()) {
        			bicNoiseFolder.mkdir();
        		} else {
        			bicNoiseFolder.delete();
        			bicNoiseFolder.mkdir();
        		}
        		
        		//
        		for (int k = 1; k <= nExperimentRepeat; k++) {
        			StringBuilder b2 = new StringBuilder();
        			b2.append(strBicNoiseFolder);
        			b2.append(Integer.toString(k));
        			b2.append("/");
        			
        			String strWorkingFolder = b2.toString();
        			File workingFolder = new File(strWorkingFolder);
        			if (!workingFolder.exists()) {
        				workingFolder.mkdir();
        			} else {
        				workingFolder.delete();
        				workingFolder.mkdir();
        			}
        			
        			//
        			CBic cbic = new CBic(binFileName,
							mulFileName,
							queryFileName,
							workingDir,
							rowThreshold,
							colThreshold,
							mdlRowColLevels,
							mdlRowColLevels,
							failureThreshold,
							restartThreshold);							
        
        			cbic.mineMultiBiclusters();
        			
        		}
        	}
        }
        
       
	}
}
