import cs.kuleuven.cp.biclusteringMDL;
//import cs.kuleuven.cp.topKMutatedPathways;

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
        Option opRowThreshold = OptionBuilder.withArgName("RowThreshold").hasArg().withDescription("row threshold").create("rth");
        Option opColThreshold = OptionBuilder.withArgName("ColThreshold").hasArg().withDescription("row threshold").create("cth");
        Option opFailureThreshold = OptionBuilder.withArgName("FailureThreshold").hasArg().withDescription("failure threshold").create("fth");
                
        Option help	= new Option("help", "Print help");
        
        Options options = new Options();
        options.addOption(opBinFile);
        options.addOption(opMulFile);
        options.addOption(opRowThreshold);
        options.addOption(opColThreshold);
        options.addOption(opFailureThreshold);
        
        options.addOption(help);
        //commandLine.
        
        
        String binFileName = "";
        String mulFileName = "";
                
        int colThreshold = 25;
        int rowThreshold = 25;
        int failureThreshold = 300;
        
                
        CommandLineParser parser = new BasicParser();
        
        try{
        	CommandLine cmd = parser.parse( options, args);
        	if (cmd.hasOption("bf")) {
        	    binFileName = cmd.getOptionValue("bf");
        	}        	
        	
        	if (cmd.hasOption("mf")) {
        		mulFileName = cmd.getOptionValue("mf");
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
    
        System.out.print("Row noise threshold = "); System.out.println(rowThreshold);
        System.out.print("Col noise threshold = "); System.out.println(colThreshold);
        System.out.print("Failure threshold = "); System.out.println(failureThreshold);
        
                
		biclusteringMDL bin = new biclusteringMDL(binFileName,
													mulFileName,
													rowThreshold,
													colThreshold,
													failureThreshold);																	;
		bin.execute();
	}
}
