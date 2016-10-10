 for i in {1..12} 
do
 data = "trip_data_${i}.csv"
 fare = "trip_fare_${i}.csv"
   if diff -w <($data | cut -d , -f 1,2,6) <($fare | cut -d , -f 1,2,4) > /dev/null; then
 echo "$i:Files matched"
 else
 echo "$i:Files do not match"
 fi
 done
