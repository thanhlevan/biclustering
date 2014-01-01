import cs.kuleuven.cp.topKMutatedPathways;
import org.apache.commons.cli.*;


public class runTopKMutatedPathways {
	
	public static void main(String[] args) {
		
        Option mutFile = OptionBuilder.withArgName("filename").hasArg().withDescription("mutation file name").create("mf");
        Option expFile = OptionBuilder.withArgName("filename").hasArg().withDescription("expression file name").create("ef");
        Option netFile = OptionBuilder.withArgName("filename").hasArg().withDescription("interaction network file name").create("nf");
        
        Option outMutFile = OptionBuilder.withArgName("filename").hasArg().withDescription("Output file of mutations").create("omf");
        Option outExpFile = OptionBuilder.withArgName("filename").hasArg().withDescription("Output file of expressed genes").create("oef");
        Option outPatFile = OptionBuilder.withArgName("filename").hasArg().withDescription("Output file of patients").create("opf");
        
        Option opMutThreshold = OptionBuilder.withArgName("value").hasArg().withDescription("Mutation threshold").create("mth");
        
        
        Option graphSearch = new Option("gs", "Search mutations with interaction network");
        Option help	= new Option("help", "Print help");
        
        Options options = new Options();
        options.addOption(mutFile);
        options.addOption(expFile);
        options.addOption(netFile);
        options.addOption(outMutFile);
        options.addOption(outExpFile);
        options.addOption(outPatFile);
        options.addOption(graphSearch);
        options.addOption(opMutThreshold);
        options.addOption(help);
        //commandLine.
        
        
        String mutFileName = "";
        String expFileName = "";
        String intFileName = "";
        
        String outMutFileName = "";
        String outExpFileName = "";
        String outPatFileName = "";
        
        int colThreshold = 25;
        int rowThreshold = 25;
        int mutThreshold = 6;
        boolean gSearch = false;
                
        CommandLineParser parser = new BasicParser();
        
        try{
        	CommandLine cmd = parser.parse( options, args);
        	if (cmd.hasOption("mf")) {
        	    mutFileName = cmd.getOptionValue("mf");
        	}        	
        	
        	if (cmd.hasOption("ef")) {
        		expFileName = cmd.getOptionValue("ef");
        	}
        	
        	if (cmd.hasOption("nf")) {
        		intFileName = cmd.getOptionValue("nf");
        	}
        	
        	if (cmd.hasOption("gs")) {
        		gSearch = true;
        	}
        	
        	if (cmd.hasOption("omf")) {
        		outMutFileName = cmd.getOptionValue("omf");        		
        	}
        	
        	if (cmd.hasOption("oef")) {
        		outExpFileName = cmd.getOptionValue("oef");
        	}
        	
        	if (cmd.hasOption("opf")) {
        		outPatFileName = cmd.getOptionValue("opf");
        	}
        	
        	if (cmd.hasOption("mth")) {
        		mutThreshold  = Integer.parseInt(cmd.getOptionValue("mth"));
        	}
        	
        	if (cmd.hasOption("help")) {
        		HelpFormatter formatter = new HelpFormatter();
        		formatter.printHelp( "runTopKMutatedPathways", options );
        		return;
        	}
        	
        }catch (ParseException ex){
        	System.out.println( ex.getMessage());
        }
        
        System.out.println("MutFile = " + mutFileName);
        System.out.println("ExpFile = " + expFileName);
        System.out.println("IntFile = " + intFileName);
        System.out.println("OutMutFile = " + outMutFileName);
        System.out.println("OutExpFile = " + outExpFileName);
        System.out.println("OutPatientFile = " + outPatFileName);
        System.out.print("Graph Search = " ); System.out.println(gSearch);
        System.out.print("Row noise threshold = "); System.out.println(rowThreshold);
        System.out.print("Col noise threshold = "); System.out.println(colThreshold);
        System.out.print("Mutation threshold = "); System.out.println(mutThreshold);
        
                
		topKMutatedPathways pathwayMiner = new topKMutatedPathways(	mutFileName,
																	expFileName,
																	intFileName,
																	outMutFileName,
																	outExpFileName,
																	outPatFileName,
																	rowThreshold,
																	colThreshold,
																	mutThreshold,
																	gSearch);
		pathwayMiner.execute();
	}
}
