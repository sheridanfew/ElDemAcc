cwd=$(pwd)

cd /Users/Shez/Google\ Drive/Grantham/ElDemAcc/CREST/Excel_Outputs

for file in *.xlsx
do
	echo $file
	file_csv=$(echo $file | sed 's/xlsx/csv/g')
	echo $file_csv
	in2csv $file > $file_csv
	rm $file
	sed 's/1899-12-30/00:00:00/g' $file_csv  > tmp
	mv tmp $file_csv
done 

for file in *.csv
do 
	# echo "$file" | sed 's/2006/Tier1/g' | sed 's/2011/Tier2/g' | sed 's/2016/Tier3/g' | sed 's/2021/Tier4/g' | sed 's/2026/Tier5/g' 
	mv $file tmp
	mv tmp $(echo "$file" | sed 's/2006/Tier1/g' | sed 's/2011/Tier2/g' | sed 's/2016/Tier3/g' | sed 's/2021/Tier4/g' | sed 's/2026/Tier5/g' )
done


cd $cwd
