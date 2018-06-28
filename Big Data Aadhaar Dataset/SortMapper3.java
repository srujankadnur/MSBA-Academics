
import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

public class SortMapper3 extends Mapper<LongWritable, Text, IntWritable, Text> {

	Text region = new Text();
	IntWritable total = new IntWritable();

	public void map(LongWritable key, Text value, Context context)
			throws IOException, InterruptedException {
		String[] splits = value.toString().split("\\|");

		region.set(splits[0].trim());
		total.set(Integer.parseInt(splits[1].trim()));

		context.write(total, region);
	}
}