

# Module texas #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-connection">connection()</a> ###


<pre><code>
connection() = #texas{}
</code></pre>




### <a name="type-connection_string">connection_string()</a> ###


<pre><code>
connection_string() = string()
</code></pre>




### <a name="type-data">data()</a> ###


<pre><code>
data() = any()
</code></pre>




### <a name="type-table">table()</a> ###


<pre><code>
table() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>
Close the connection to the database.</td></tr><tr><td valign="top"><a href="#connect-0">connect/0</a></td><td>
Connect to the database.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td></td></tr><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#create_table-2">create_table/2</a></td><td>
Create the given table (if not exists).</td></tr><tr><td valign="top"><a href="#create_table-3">create_table/3</a></td><td>
Create the given table (if not exists).</td></tr><tr><td valign="top"><a href="#driver-1">driver/1</a></td><td></td></tr><tr><td valign="top"><a href="#drop_table-2">drop_table/2</a></td><td>
Drop the given table (if exists).</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_keylist-2">to_keylist/2</a></td><td>
Return the record as keylist.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Conn::<a href="#type-connection">connection()</a>) -&gt; any()
</code></pre>
<br />

Close the connection to the database

<a name="connect-0"></a>

### connect/0 ###

<pre><code>
connect() -&gt; <a href="#type-connection">connection()</a> | {error, any()}
</code></pre>
<br />

Connect to the database.

<a name="connect-1"></a>

### connect/1 ###

<pre><code>
connect(URI::<a href="#type-connection_string">connection_string()</a>) -&gt; <a href="#type-connection">connection()</a> | {error, any()}
</code></pre>
<br />

<a name="connect-2"></a>

### connect/2 ###

<pre><code>
connect(URI::<a href="#type-connection_string">connection_string()</a>, Options::[tuple()]) -&gt; <a href="#type-connection">connection()</a> | {error, any()}
</code></pre>
<br />

<a name="create_table-2"></a>

### create_table/2 ###

<pre><code>
create_table(Conn::<a href="#type-connection">connection()</a>, Table::<a href="#type-table">table()</a>) -&gt; any()
</code></pre>
<br />

Create the given table (if not exists)

<a name="create_table-3"></a>

### create_table/3 ###

<pre><code>
create_table(Conn::<a href="#type-connection">connection()</a>, Table::<a href="#type-table">table()</a>, Fields::list()) -&gt; any()
</code></pre>
<br />

Create the given table (if not exists)

<a name="driver-1"></a>

### driver/1 ###

<pre><code>
driver(Conn::<a href="#type-connection">connection()</a> | <a href="#type-data">data()</a>) -&gt; atom()
</code></pre>
<br />

<a name="drop_table-2"></a>

### drop_table/2 ###

<pre><code>
drop_table(Conn::<a href="#type-connection">connection()</a>, Table::<a href="#type-table">table()</a>) -&gt; ok | error
</code></pre>
<br />

Drop the given table (if exists)

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; ok | {ok, <a href="#type-connection">connection()</a>} | {error, any()}
</code></pre>
<br />

<a name="to_keylist-2"></a>

### to_keylist/2 ###

<pre><code>
to_keylist(Table::atom(), Record::<a href="#type-data">data()</a>) -&gt; list()
</code></pre>
<br />

Return the record as keylist

