#include<iostream>
#include<fstream>
#include<math.h>
#include<string>
#include<vector>

using namespace std;

// This function sparse a string by a delimiter. It has been downloaded from:
// http://www.sbin.org/doc/HOWTO/C++Programming-HOWTO-7.html
void Tokenize(const string& str, vector<string>& tokens, const string& delimiters = " ")
{
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

int main ()
{
    std::ifstream file("data_list.txt");
    std::string str;

    vector< vector <int> > stars_profile;
    vector<int> num_stars;

    int num = 0;
    while (std::getline(file, str))
    {
	vector<string> tokens;
	Tokenize(str,tokens,"\t");
	vector<int> row;
	for ( int i = 0 ; i < tokens.size() ; i++ )
	{
	    int m = atoi(tokens[i].c_str());
	    row.push_back ( m );
	    if ( m < num_stars.size() )
	    {
		num_stars[m] = num_stars[m] + 1;
	    }
	    else
	    {
		while ( num_stars.size() < m )
		{
		    num_stars.push_back ( 0 );
		}
		num_stars.push_back ( 1 );
	    }
	}
	stars_profile.push_back ( row );
	cout << num << ':' << tokens.size() << endl;
	num++;
    }
    
    int num_m = num_stars.size();

    vector < vector <int> > movies_connection;
    for ( int i = 0 ; i < num_m ; i++ )
    {
	vector < int > row;
	movies_connection.push_back ( row );
    }
    
    for ( int i = 0 ; i < num ; i++ )
    {
	cout << i << '/' << num << endl;
	for ( int s1 = 0 ; s1 < stars_profile[i].size() ; s1++ )
	{
	    for ( int s2 = (s1+1) ; s2 < stars_profile[i].size() ; s2++ )
	    {
		movies_connection[stars_profile[i][s1]].push_back ( stars_profile[i][s2] );
		movies_connection[stars_profile[i][s2]].push_back ( stars_profile[i][s1] );
	    }
	}
    }

    std::ofstream out("network_of_movies_smaller.txt");

    for ( int i = 0 ; i < num_m ; i++ )
    {
	cout << i << '/' << num_m << endl;
	if ( num_stars[i] >= 25 )
	{
	    vector<int> end_nodes;
	    vector<int> num_common;
	    for ( int j = 0 ; j < movies_connection[i].size() ; j++ )
	    {
		if ( ( i < movies_connection[i][j] ) && ( num_stars[movies_connection[i][j]] >= 25 ) )
		{
		    std::vector<int>::iterator it = find(end_nodes.begin(), end_nodes.end(), movies_connection[i][j]);
		    if ( it == end_nodes.end() )
		    {
			end_nodes.push_back ( movies_connection[i][j] );
			num_common.push_back ( 1 );
		    }
		    else
		    {
			int pos = it - end_nodes.begin();
			num_common[pos] = num_common[pos] + 1;
		    }
		}
	    }
	    for ( int j = 0 ; j < end_nodes.size() ; j++ )
	    {
		double weight = num_common[j];
		double den = num_stars[i] + num_stars[end_nodes[j]] - num_common[j];
		weight = weight / den;
		out << i << '\t' << end_nodes[j] << '\t' << weight << endl;
	    }
	}
    }
    out.close();

    return 0;
}
