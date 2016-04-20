/**
 * Disclaimer:
 * This file is an example on how to use the Cassandra SSTableSimpleUnsortedWriter class to create
 * sstables from a csv input file.
 * While this has been tested to work, this program is provided "as is" with no guarantee. Moreover,
 * it's primary aim is toward simplicity rather than completness. In partical, don't use this as an
 * example to parse csv files at home.
 */
import java.nio.ByteBuffer;
import java.io.*;
import java.util.UUID;

import org.apache.cassandra.db.marshal.*;
import org.apache.cassandra.io.sstable.SSTableSimpleUnsortedWriter;
import static org.apache.cassandra.utils.ByteBufferUtil.bytes;
import static org.apache.cassandra.utils.UUIDGen.decompose;
import org.apache.cassandra.dht.IPartitioner;
//import org.apache.cassandra.dht.RandomPartitioner;
import org.apache.cassandra.dht.Murmur3Partitioner;

public class DataImportExample1
{
    static String filename;

    public static void main(String[] args) throws IOException
    {
        /*if (args.length == 0)
        {
            System.out.println("Expecting <csv_file> as argument");
            System.exit(1);
        }*/
        filename = "/home/ashish/USDJPY-2009-05.csv";
        BufferedReader reader = new BufferedReader(new FileReader(filename));

        String keyspace = "demo1";
        File directory = new File(keyspace);
        if (!directory.exists())
            directory.mkdir();

           //System.out.println(directory);
      IPartitioner partitioner = new Murmur3Partitioner();

        SSTableSimpleUnsortedWriter usersWriter = new SSTableSimpleUnsortedWriter(directory,partitioner,keyspace,"Users3",AsciiType.instance,null,64);

        String line;
        int lineNumber = 1;
        CsvEntry entry = new CsvEntry();
        // There is no reason not to use the same timestamp for every column in that example.
        long timestamp = System.currentTimeMillis() * 1000;
        while ((line = reader.readLine()) != null)
        {
            if (entry.parse(line, lineNumber))
            {   
                //usersWriter.newRow(uuid);
                usersWriter.newRow("");
                usersWriter.addColumn(bytes("symbol"), bytes(entry.symbol), timestamp);
                usersWriter.addColumn(bytes("timestamp1"), bytes(entry.timestamp1), timestamp);
                usersWriter.addColumn(bytes("Bid_Price"), bytes(entry.Bid_Price), timestamp);
                usersWriter.addColumn(bytes("Ask_Price"), bytes(entry.Ask_Price), timestamp);
            }
            lineNumber++;
        }
        // Don't forget to close!
        usersWriter.close();
        System.exit(0);
    }

    static class CsvEntry
    {
        //UUID CorrelationID;
        String symbol;
        String timestamp1;
        String Bid_Price;
        String Ask_Price;

        boolean parse(String line, int lineNumber)
        {
            // Ghetto csv parsing
            String[] columns = line.split(",");
            if (columns.length != 4)
            {
                System.out.println(String.format("Invalid input '%s' at line %d of %s", line, lineNumber, filename));
                return false;
            }
            try
            {
                symbol = columns[0].trim();
                timestamp1 = columns[1].trim();
                Bid_Price= columns[2].trim();
                Ask_Price = columns[3].trim();

                return true;
            }
            catch (NumberFormatException e)
            {
                System.out.println(String.format("Invalid number in input '%s' at line %d of %s", line, lineNumber, filename));
                return false;
            }
        }
    }
}

