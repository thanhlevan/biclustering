import cs.kuleuven.cp.biclusteringMDL;
import cs.kuleuven.cp.CBic;


import org.apache.commons.cli.*;

/**
 * Created on December 27, 2013
 * @author thanhle
 *
 */
public class runBiclusteringMDL {

	public static void main(String[] args) {
		
		Option opBinFile = OptionBuilder.withArgName("BinaryFile").hasArg().withDescription("binary (duplicated) file name").create("bf");
        Option opMulFile = OptionBuilder.withArgName("MulFile").hasArg().withDescription("multi-valued file name").create("mf");
        Option opQueryFile = OptionBuilder.withArgName("QueryFile").hasArg().withDescription("query file name").create("qf");
        Option opRowThreshold = OptionBuilder.withArgName("RowThreshold").hasArg().withDescription("row threshold").create("rth");
        Option opColThreshold = OptionBuilder.withArgName("ColThreshold").hasArg().withDescription("row threshold").create("cth");
        Option opFailureThreshold = OptionBuilder.withArgName("FailureThreshold").hasArg().withDescription("failure threshold").create("fth");
        Option opRestartThreshold = OptionBuilder.withArgName("RestartThreshold").hasArg().withDescription("restart threshold").create("sth");
        Option opRemovedRowsFile = OptionBuilder.withArgName("RemovedRowsFile").hasArg().withDescription("File contain rows need to be removed").create("rf");
        Option opWorkingDir = OptionBuilder.withArgName("WorkingDirectory").hasArg().withDescription("Working directory to store results").create("wd");
                
        Option help	= new Option("help", "Print help");
        
        Options options = new Options();
        options.addOption(opBinFile);
        options.addOption(opMulFile);
        options.addOption(opQueryFile);
        options.addOption(opRowThreshold);
        options.addOption(opColThreshold);
        options.addOption(opFailureThreshold);
        options.addOption(opRestartThreshold);
        options.addOption(opRemovedRowsFile);
        options.addOption(opWorkingDir);
                
        options.addOption(help);
        
        // default values
                
        String binFileName 		= "/home/thanh/data/biclusters/8modules/data/noise_10/dup_bg_0.10_bic_0.10_8modules.txt" ;
        String mulFileName 		= "/home/thanh/data/biclusters/8modules/data/noise_10/bg_0.10_bic_0.10_8modules.txt" ;
        String queryFileName 	= "/home/thanh/querytest.txt" ;
        String rmRowsFileName 	= "" ;
        String workingDir 		= "/home/thanh/test/syn/query_450/" ;
                
        int colThreshold = 25;
        int rowThreshold = 25;
        int failureThreshold = 300;
        int restartThreshold = 30;
                        
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
        	
        }catch (ParseException ex){
        	System.out.println( ex.getMessage());
        }
        
        System.out.println("Binary File = " + binFileName);
        System.out.println("Multivalued File = " + mulFileName);
        System.out.println("Query File = " + queryFileName);
        System.out.println("Removed rows File = " + rmRowsFileName);
    
        System.out.print("Row noise threshold = "); System.out.println(rowThreshold);
        System.out.print("Col noise threshold = "); System.out.println(colThreshold);
        System.out.print("Failure threshold = "); System.out.println(failureThreshold);
        System.out.print("Restart threshold = "); System.out.println(restartThreshold);
        
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
        int solutionIndex = 1;
        /*CBic bic = new CBic(binFileName,
													mulFileName,
													queryFileName,
													//rmRowsFileName,
													workingDir,
													rowThreshold,
													colThreshold,
													failureThreshold,
													restartThreshold
													);
													//solutionIndex);	
        bic.mineMultiBiclusters();*/
		/*biclusteringMDL bin = new biclusteringMDL(binFileName,
													mulFileName,
													queryFileName,
													rmRowsFileName,
													workingDir,
													rowThreshold,
													colThreshold,
													failureThreshold,
													restartThreshold,
													solutionIndex);																	;
		bin.execute();
		if (!bin.isSolutionFound())
			return;
		
		boolean bStop = false;
		while (!bStop) {
			solutionIndex++;
			System.out.println("*********************************************");
			System.out.println("* Starting the " + Integer.toString(solutionIndex) + "th iteration      *");
			System.out.println("*********************************************");
			
			rmRowsFileName = workingDir + "removedRows.txt";
			biclusteringMDL biMiner = new biclusteringMDL(binFileName,
					mulFileName,
					queryFileName,
					rmRowsFileName,
					workingDir,
					rowThreshold,
					colThreshold,
					failureThreshold,
					restartThreshold,
					solutionIndex);	
			biMiner.execute();
			
			if (!biMiner.isSolutionFound()) {
				bStop = true;
				System.out.println("No solution found in the iteration " + solutionIndex);
				System.out.println("Stop.");
			}
			
			if (!biMiner.bBetterMDLScore()) {
				bStop = true;
				System.out.println("The MDL score will get worse when adding the solution found in this iteration!");
				System.out.println("Program terminated!");
			}
		}*/
	}
}
