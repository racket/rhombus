#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/date)

@(def date_eval: make_rhombus_eval())
@examples(
  ~hidden: #true
  ~eval: date_eval
  import rhombus/date
)

@title(~style: #'toc, ~tag: "date"){Date and Time}

@docmodule(rhombus/date)

The @rhombusmodname(rhombus/date) library provides basic facilities for
working with dates and times. A @rhombus(date.Time, ~class) represents
just a time (relative to an unspecified date and time zone),
@rhombus(date.Date, ~class) represents just a day (relative to an
unspecified time zone), a @rhombus(date.DateTime, ~class) has both a
day and time, and a @rhombus(date.ZonedDateTime, ~class) represents an
absolute point in time as a date and time in a specific time zone.

@local_table_of_contents()

@// ============================================================

@section(~tag: "date-time"){Time}

@doc(
  class date.Time(~hour: hour :: Int.in(0, 23) = 0,
                  ~minute: minute :: Int.in(0, 59) = 0,
                  ~second: second :: Int.in(0, 60) = 0,
                  ~nanosecond: nanosecond :: Int.in(0, 999_999_999) = 0)
){

 Represents a time relative to an unspecified date and in an unspecified
 time zone.

 The @rhombus(date.Time, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.Time, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).

@examples(
  ~eval: date_eval
  ~repl:
    def tm = date.Time(~hour: 5, ~minute: 45)
    tm
    tm.to_string()
    tm < date.Time(~hour: 6, ~minute: 10)
)

}

@doc(
  fun date.Time.now(~local: local :: Any = #true) :: date.Time
){

 Uses the system clock via @rhombus(system.milliseconds) to get the
 current time and using the operating system's facilities to get a time
 of day relative to either the current time zone (when @rhombus(local) is
 true) or UTC (when @rhombus(local) is false).

}


@doc(
  fun date.Time.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.Time
){

 Uses operating system facilities to convert a number of seconds since
 @tech{the epoch} to a time of day relative to either the current time
 zone (when @rhombus(local) is true) or UTC (when @rhombus(local) is
 false).

@examples(
  ~eval: date_eval
  ~repl:
    date.Time.from_seconds(0, ~local: #false)
)

}


@doc(
  method (tm :: date.Time).to_string(
    ~show_nanosecond: show_nanosecond :: Any = #false
  ) :: String
){

 Converts @rhombus(tm) to a string format
 @tt{HH@litchar{:}MM@litchar{:}SS},
 @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSS},
 @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSSSSS},
 @tt{HH@litchar{:}MM@litchar{:}SS@litchar{.}SSSSSSSSS}, depending on the
 precision needed to report the nanoseconds of @rhombus(tm). If
 @rhombus(show_nanosecond) is @rhombus(#false), the result is always
 of the form @tt{HH@litchar{:}MM@litchar{:}SS}.

@examples(
  ~eval: date_eval
  ~repl:
    def tm = date.Time(~hour: 5, ~minute: 45, ~second: 1, ~nanosecond: 70000)
    tm.to_string()
    tm.to_string(~show_nanosecond: #true)
)

}

@// ============================================================

@section(~tag: "date-date"){Date}

@doc(
  class date.Date(~year: year :: Int,
                  ~month: month :: Int.in(1, 12),
                  ~day: day :: Int.in(1, 31))
){

 Represents a date relative to an unspecified time zone.

 The @rhombus(date.Date, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.Date, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.Date(~year: 1968, ~month: 1, ~day: 24)
    dt
    dt.to_string()
    dt < date.Date(~year: 1969, ~month: 7, ~day: 20)
)

}

@doc(
  fun date.Date.now(~local: local :: Any = #true) :: date.Date
){

 Uses the system clock via @rhombus(system.milliseconds) to get the
 current time and using the operating system's facilities to get a date
 relative to either the current time zone (when @rhombus(local) is true)
 or UTC (when @rhombus(local) is false).

}


@doc(
  fun date.Date.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.Date
){

 Uses operating system facilities to convert a number of seconds since
 @tech{the epoch} to a date relative to either the current time
 zone (when @rhombus(local) is true) or UTC (when @rhombus(local) is
 false).

@examples(
  ~eval: date_eval
  ~repl:
    date.Date.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.Date).to_string(
    ~format: format :: date.Format = #'rfc3339
  ) :: String
){

 Converts @rhombus(dt) to a string using the format selected by
 @rhombus(format).

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.Date(~year: 1968, ~month: 1, ~day: 24)
    dt.to_string()
    dt.to_string(~format: #'american)
)

}


@doc(
  method (dt :: date.Date).to_datetime(
    ~time: tm :: date.Time = Time()
  ) :: DateTime
){

 Converts a @rhombus(date.Date) to a @rhombus(date.DateTime) using
 @rhombus(tm) for the time portion of the new @rhombus(date.DateTime),
 and using UTC as the time zone.

}

@doc(
  method (dt :: date.Date).to_seconds() :: Real
){

 Equivalent to @rhombus(tm.to_datetime().to_seconds()).

}

@// ============================================================

@section(~tag: "date-datetime"){Date and Time Combined}

@doc(
  class date.DateTime(
    ~year: year :: Int,
    ~month: month :: Int.in(1, 12),
    ~day: day :: Int.in(1, 31),
    ~hour: hour :: Int.in(0, 23) = 0,
    ~minute: minute :: Int.in(0, 59) = 0,
    ~second: second :: Int.in(0, 60) = 0,
    ~nanosecond: nanosecond :: Int.in(0, 999_999_000) = 0
  )
){

 Combines the fields of @rhombus(date.Date) and @rhombus(date.Time) to
 represent a specific date and time relative to an unspecified time zone.

 The @rhombus(date.DateTime, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.DateTime, ~annot) instances
 can be compared with operators like @rhombus(<) and @rhombus(>).

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.DateTime(~year: 2025, ~month: 11, ~day: 14,
                           ~hour: 18, ~minute: 14)
    dt
    dt.to_string()
    dt > date.DateTime.from_seconds(0)
)

}


@doc(
  fun date.DateTime.now(~local: local :: Any = #true) :: date.DateTime
){

 Like @rhombus(date.Date.now), but preserving the current time within
 the current day.

}


@doc(
  fun date.DateTime.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.TimeDate
){

 Like @rhombus(date.Date.now), but preserving the time of @rhombus(secs)
 within the day of @rhombus(secs).

@examples(
  ~eval: date_eval
  ~repl:
    date.DateTime.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.DateTime).to_string(
    ~format: format :: date.Format = #'rfc3339,
    ~show_time: show_time :: Any = #true,
    ~show_nanosecond: show_nanosecond :: Any = #false
  ) :: String
){

 Converts @rhombus(dt) to a string using the format selected by
 @rhombus(format). If @rhombus(show_time) is true, then the string
 includes the time portion of @rhombus(dt). If @rhombus(show_nanosecond)
 is also true, then the time portion includes nanoseconds if supported by
 the format. If a format requires a time zone, then UTC is used.
 Depending on @rhombus(format), @rhombus(dt) may be converted to a
 @rhombus(date.ZonedDateTime) to produce the string.

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.DateTime(~year: 1968, ~month: 1, ~day: 24, ~second: 10)
    dt.to_string()
    dt.to_string(~format: #'rfc2822)
)

}


@doc(
  method (dt :: date.DateTime).to_zoned() :: ZonedDateTime
){

 Converts a @rhombus(date.DateTime) to a @rhombus(date.ZonedDateTime)
 using using UTC as the time zone. This conversion can fail if the date
 falls outside the (large) range of dates that the operating system can
 locate.

}

@doc(
  method (dt :: date.DateTime).to_seconds() :: Real
){

 Equivalent to @rhombus(tm.to_zoned().to_seconds()).

}

@// ============================================================

@section(~tag: "date-zoneddatetime"){Date and Time in a Time Zone}

@doc(
  class date.ZonedDateTime(
    ~year: year :: Int,
    ~month: month :: Int.in(1, 12),
    ~day: day :: Int.in(1, 31),
    ~hour: hour :: Int.in(0, 23) = 0,
    ~minute: minute :: Int.in(0, 59) = 0,
    ~second: second :: Int.in(0, 60) = 0,
    ~nanosecond: nanosecond :: Int.in(0, 999_999_999) = 0,
    ~week_day: week_day :: Int.in(0, 6) = 0,
    ~year_day: year_day :: Int.in(0, 365) = 0,
    ~is_dst: is_dst :: Boolean = #false,
    ~time_zone_offset: time_zone_offset :: Int = 0,
    ~time_zone_name: time_zone_name :: String = "UTC"
  )
){

 Represents an absolute date and time within a specific time zone. The
 @rhombus(time_zone_offset) argument is in seconds. There is no checking
 that @rhombus(week_day) and @rhombus(year_day) are consistent with other
 fields, but they will be filled in correctly when a function like
 @rhombus(date.ZonedDateTime.now)
 @rhombus(date.ZonedDateTime.from_seconds) is used to create a
 @rhombus(date.ZonedDateTime, ~class).

 The @rhombus(date.DateTime, ~annot) class privately implements
 @rhombus(Comparable, ~class), so @rhombus(date.ZonedDateTime, ~annot)
 instances can be compared with operators like @rhombus(<) and
 @rhombus(>). Comparison involves conversion via
 @rhombus(date.ZonedDateTime.to_seconds).

}

@doc(
  fun date.ZonedDateTime.now(~local: local :: Any = #true) :: date.DateTime
){

 Like @rhombus(date.DateTime.now), but preserving the time zone selected
 by @rhombus(local) and (if true) the current time zone.

}


@doc(
  fun date.ZonedDateTime.from_seconds(
    secs :: Real,
    ~local: local :: Any = #true
  ) :: date.TimeDate
){

 Like @rhombus(date.DateTime.from_seconds), but preserving the time zone
 selected by @rhombus(local) and (if true) the current time zone.

@examples(
  ~eval: date_eval
  ~repl:
    date.ZonedDateTime.from_seconds(0, ~local: #false)
)

}


@doc(
  method (dt :: date.ZonedDateTime).to_string(
    ~format: format :: date.Format = #'rfc3339,
    ~show_time: show_time :: Any = #true,
    ~show_nanosecond: show_nanosecond :: Any = #false,
    ~show_time_zone: show_time_zone :: Any = #true
  ) :: String
){

 Converts @rhombus(dt) to a string in a similar way as
 @rhombus(date.DateTime), but with an extra option
 @rhombus(show_time_zone) to determine whether a time zone is shown if
 it is optional for @rhombus(format).

@examples(
  ~eval: date_eval
  ~repl:
    def dt = date.ZonedDateTime(~year: 1968, ~month: 1, ~day: 24, ~second: 10,
                                ~time_zone_offset: 7 * 60 * 60,
                                ~time_zone_name: "MDT")
    dt.to_string()
    dt.to_string(~format: #'rfc2822)
)

}


@doc(
  method (dt :: date.ZonedDateTime).to_seconds() :: Real
){

 Returns the number of seconds since @tech{the epoch} until
 @rhombus(dt).

}

@// ============================================================

@section(~tag: "date-format"){Date Formats}

@doc(
  enum date.Format:
    rfc2822
    rfc3339
    iso8601
    american
    european
    julian
){

 A format supported by @rhombus(date.Date.to_string),
 @rhombus(date.DateTime.to_string), and
 @rhombus(date.ZonedDateTime.to_string):

@itemlist(

  @item{@rhombus(#'rfc2822): follows
   @hyperlink("https://datatracker.ietf.org/doc/html/rfc2822"){RFC 2822}.}

  @item{@rhombus(#'rfc3339): follows
   @hyperlink("https://datatracker.ietf.org/doc/html/rfc3339"){RFC 3339},
   specifically using a space character between a date and time, and
   showing a time zone offset (if requested) as @litchar{Z},
   @tt{@litchar{+}HH@litchar{:}MM} or @tt{@litchar{+}HH@litchar{:}MM}.}

  @item{@rhombus(#'iso8601): follows ISO 8601, showing a time zone offset
   (if requested) as @litchar{Z}, @tt{@litchar{+}HH@litchar{:}MM}, or
   @tt{@litchar{+}HH@litchar{:}MM}.}

  @item{@rhombus(#'american): shows a date in an English, human-readable
   form that spells out a month and day of the week, putting the month
   before the day.}

  @item{@rhombus(#'european): shows a date in an English, human-readable
   form that spells out a month and day of the week, putting the date
   before the month.}

  @item{@rhombus(#'julian): shows the date in Julian form.}

)

}
