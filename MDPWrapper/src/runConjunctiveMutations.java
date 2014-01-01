import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import cs.kuleuven.cp.ConjunctiveMutations;

public class runConjunctiveMutations {
	public static void main(String[] args) {
		
		Option mutFile = OptionBuilder.withArgName("filename").hasArg().withDescription("mutation file name").create("mf");                
        Option outMutFile = OptionBuilder.withArgName("filename").hasArg().withDescription("Output file of mutations").create("omf");
                
        Option opMutThreshold = OptionBuilder.withArgName("value").hasArg().withDescription("Mutation threshold").create("mth");
        Option opRowNoiseLevel = OptionBuilder.withArgName("value").hasArg().withDescription("Row noise threshold").create("rth");
        Option opColNoiseLevel = OptionBuilder.withArgName("value").hasArg().withDescription("Column noise threshold").create("cth");
        
        Option help	= new Option("help", "Print help");
        Option opLns = new Option("lns", "use large neighbourhood search");
        
        Options options = new Options();
        options.addOption(mutFile);
        options.addOption(outMutFile);             
        options.addOption(opMutThreshold);
        options.addOption(opRowNoiseLevel);
        options.addOption(opColNoiseLevel);
        options.addOption(help);
        options.addOption(opLns);
        //commandLine.
                
        String mutFileName = "";                
        String outMutFileName = "";
        boolean bLns = false;
                
        int colThreshold = 25;
        int rowThreshold = 25;
        int mutThreshold = 6;        
                
        CommandLineParser parser = new BasicParser();
        
        try{
        	CommandLine cmd = parser.parse( options, args);
        	if (cmd.hasOption("mf")) {
        	    mutFileName = cmd.getOptionValue("mf");
        	}        	
        	
        	if (cmd.hasOption("omf")) {
        		outMutFileName = cmd.getOptionValue("omf");        		
        	}
        
        	if (cmd.hasOption("mth")) {
        		mutThreshold  = Integer.parseInt(cmd.getOptionValue("mth"));
        	}
        	
        	if (cmd.hasOption("cth")) {
        		colThreshold  = Integer.parseInt(cmd.getOptionValue("cth"));
        	}
        	
        	if (cmd.hasOption("mth")) {
        		rowThreshold  = Integer.parseInt(cmd.getOptionValue("rth"));
        	}
        	
        	if (cmd.hasOption("help")) {
        		HelpFormatter formatter = new HelpFormatter();
        		formatter.printHelp( "runConjunctiveMutations", options );
        		return;
        	}
        	
        	if (cmd.hasOption("lns")) {
        		bLns = true;
        	}
        	
        }catch (ParseException ex){
        	System.out.println( ex.getMessage());
        }
        
        System.out.println("MutFile = " + mutFileName);        
        System.out.println("OutMutFile = " + outMutFileName);
        System.out.print("Row noise threshold = "); System.out.println(rowThreshold);
        System.out.print("Col noise threshold = "); System.out.println(colThreshold);
        System.out.print("Mutation threshold = "); System.out.println(mutThreshold);
        System.out.print("Use large neigbourhood search = "); System.out.println(bLns);
        
		
		ConjunctiveMutations cm = new ConjunctiveMutations(mutFileName, // mutation file
				"", //output file        					
				rowThreshold, // mRowThreshold: Integer,
				colThreshold, // mColThreshold: Integer,
				mutThreshold	// mutThreshold: Integer  
				) ;
		if (bLns) {
			cm.executeLNS();
		} else {
			cm.execute();
		}
	}
}
