#lang racket

(require racket/async-channel
         racket/random
         (rename-in (except-in racket/base
                               date?
                               date)
                    (map racket/base/map)
                    (filter racket/base/filter))
         graph
         gregor
         gregor/period
         (except-in wizard contains?))

(require "core.rkt"
         "util.rkt"
         "repl.rkt"
         "print.rkt"
         "systems/time.rkt"
         "systems/anatomy.rkt"
         "systems/map.rkt"
         "verbs/look.rkt")

#|

How can we handle "scope" and figuring out what the user is referring to? Can we
generate a set of aliases for objects? What order do we search for things in,
and when do we decide whether to ask for disambiguation vs assuming we've got
the correct thing?

Everything in the current area.
Everything on the player's person.

For some kinds of actions, we might want to talk about other things. Perhaps it
doesn't matter whether there is line of sight for a particular
action (discussing it perhaps?).

For every "thing", we can build an index based on the words in "name". We can
define aliases. Certain types of components may themselves act as adjectives.
Consider a "container". That's a component which, when added to something imbues
it with the ability to contain things. The player may refer to the thing in
question as a container regardless of what it is actually called. Our "chest"
entity might legitimately be called "container" by the player.

If there's an adjective-providing component, we may want to surpress that
adjective on a case-by-case basis, in the event that the container-ness is an
implementation detail for the entity in question.

So a score is calculated like this:

Full name match is worth the most.

Individual words are worth a fraction based on how many words were given.

Adjectives contributed otherwise are worth less still.

Multipliers are applied based on the proximity of the entity to the player (or
actor).

How long/far do we search? Until we find something reasonably good? We can't
know we're done unless we search everywhere or maintain an index of everything.

|#


(define chest (define/entity
                (@thing "Chest" "This is our example container.")
                (@container)))

(define fork (define/entity
               (@thing "Fork" "A small silver fork with four tines.")))

(chest . contains . fork)

;; Given a symbol and a string suffix, returns a symbol consisting of the
;; original symbol, with the provided string appended to the end. (In other
;; words it does exactly what you think it does).
;; An example of an event with no associated data.
#;(define/rule #:for event #:basis (&time::advanced)
  #:rule (_ (printf "Time passes...")))

;; This works, but the semantics of "abide-rulebook" are a little confusing.
;; -------------------------------------- Lab
(define player (define/entity
                 (@animal human-graph)
                 (@thing "You"
                         "You're kind of ugly. But at least you're rich.")
                 (@actor)
                 (@container)))

(define starting-room
  (define/entity
    (@room "Starting Room"
           "This is the description of the starting room.")
    (@container)))

(player . contained-by . starting-room)

(*current-actor* player)
;; (define/rule
;;   #:for event/after-turn
;;   #:rule (_ (print-current-room-desc)))


;; ------- Surfaces

(define table (define/entity
                (@thing "Table" "A short wooden table.")
                (@surface)))

(table . supports . chest)

;; -------- Inventory

;; -------- Description helpers

;; ---------- something better ---------------

;; Let's build that Skyrim adventure game. Then lets do it on e-ink.

;; The world is represented using a discrete quadtree variant that allows for
;; quadrants of varying sizes (as most quadtrees do). The discrete aspect means
;; that there is in fact a "small as you can go", but it also means that big
;; regions can be represented the same way as small ones. No need to have the
;; player wander through 500 identical rooms just to get to the other side.

;; On the other hand, we need to be able to define exits to each region. A big
;; region surrounded by small regions would cause the big region to have
;; potentially many exits on a single side.

;; ----------- A second approach

;; The world itself is sort of fractal. At the high level, travel is done
;; between regions (countries or continents even). The player is also
;; simultaneously inhabiting (at least conceptually) smaller sub-regions.
;; Perhaps a single forest clearing or something like that. The scale could go
;; perhaps several times smaller, until the player is navigating through parts
;; of a room.

;; Travel through these "fractal layers" could be automatic. Depending on the
;; specificity of the location the player is attempting to navigate to. If they
;; say "travel to whiterun", then we know they're attempting to travel in terms
;; of the regional layer (in which whiterun is presumed to be a named location).

;; We could make the grid explicit, and allow the player to refer to cells using
;; "battleship notation". This seems clunky but it would actually do a lot
;; towards making the game feel more "pen and paper". Imagine a dungeon where
;; many locations are unknown, navigating through the halls using this kind of
;; notation. Making notes in the journal about the locations of points of
;; interest.

;; Not all dungeon rooms need be the same size in this scheme. I'm not even sure
;; that they need to be squares or rectangles. There are additional tactical
;; implications to this sort of grid approach, though it will take some care to
;; make sure it doesn't overtake the actual game (referring to the interactive
;; fiction element).

;; ^^^ But this solves other problems! Particularly how to make ranged combat
;; (or combat in general) interesting. The player gets back the spatial
;; awareness of roguelikes and table top games, while having the running
;; transcript of the action where the vastly more detailed world of IF can be
;; brought into the light.

;; --------------- Back to the data

;; So what we want is a sort of quad-tree with a discrete set of levels. Each
;; element in the quadtree belongs to a sort of hierarchy. At the bottom level,
;; that's where you're most specifically standing. But higher up it might refer
;; to the dungeon you're in, and then the region the dungeon is in, and perhaps
;; the country that the region is a part of (or even the plane of existence).

;; This quadtree will not need to be optimized for adding or removing anything,
;; as the map data is unlikely to change at all (and not often at any rate).

;; node refers to the gamemap that the entity currently occupies. x and y refer
;; to the specific location within that map. Things don't necessarily have a
;; position if they are (for example) contained in something else.
(define/component position (node x y))

(struct GameMap (locid [data #:mutable] parent) #:transparent)

(define (init-worldmap)
  (GameMap 0 #f #f))

(define *current-position* (make-parameter (init-worldmap)))

;; Given a gamemap, generate the world data for it. This will generally involve
;; building a series of locations, and adding them as nodes to a hash table. The
;; node itself is updated in place. It is possible that we could generate map
;; data on an as-needed basis using this mechanism.
(define (generate-gamemap gamemap generator dimensions)
  (set-GameMap-data! gamemap (generator gamemap dimensions)))

;; TODO: Define a generic thingy for map generators

(define (simple-generator location)
  ;; Returns a function which generates a map with the same location at each
  ;; position. I'm not sure the rammifications of that since we haven't
  ;; implemented moving around.
  (λ (gamemap dimensions)
    (let ([data (make-hash)])
      (for ([x (in-range 0 dimensions)])
        (for ([y (in-range 0 dimensions)])
          (hash-update! data x (λ (v) (cons (GameMap location #f gamemap) v)) '())))
      data)))

(define test-loc
  (define/entity
    (@room "Test" "This is a test room.")))

(generate-gamemap (*current-position*)
                  (simple-generator test-loc)
                  2)

;; --------------- Getting ahead of ourselves...

#|


Let's work out the basic details of a compelling medieval-eqsue world
simulation. We will start from the bottom, but first we have to find it.

|#



;; Some demo timepieces demonstrating how we can show the time differently in
;; different contexts.

(define sun (define/entity (@timepiece "'some time' b")))

;; FIXME: You shouldn't be able to use the sundial at night (or really past
;; sundown).
;; FIXME: You shouldn't be able to use the sundial when it's overcast.
(define sundial (define/entity
                  (@thing "Sun Dial" "A stone sun dial consisting of a rough slab of granite and a rusted iron gnomon protruding from the center.")
                  (@timepiece "'around' h b")))

(define clocktower (define/entity
                     (@thing "Clock Tower" "Stretching high into the sky, the stone clock tower casts a commanding shadow over the ground below.")
                     (@timepiece "h:mm a")))

(define calendar (define/entity
                   (@thing "Calendar" "A calendar made from a thick parchment. This calendar appears to be well kept, with past dates meticulously recorded.")
                   (@timepiece "EEEE, MMMM dd, yyyyG")))

;; ---- The passage of time

#|

In the previous section we added the notion of time to the world. We defined a
series of time and date keeping devices, each with their own time-keeping
precision.

Now we implement the passage of time. This is a complicated notion because we
have two different "modes" for the passage of time. First is the realtime mode
where time moves forward at a constant rate. Actions taken by the player (and
anyone else) take time to complete. Actions are received by checking for events
at the start of the gameloop. The gameloop is essentially running in a separate
thread to the main REPL.

This means that actions are sent to the other thread via a channel.

|#


(define (update-fn)
  (do-action #f))

(start-repl #:on-update update-fn)

;; ------------ Saving memory with implied stuff

#|

When you're in an area outside, there is assumed to be dirt and rocks on the
ground. We don't have to actually build them out and include them, but if the
player interacts with them, we need to be able to handle that.

This is really part of the whole "scope" contept - when an entity is in scope,
it can bring with it a bunch of references to other things which can be assumed
to be present.

Related to that is the idea of clutter. Clutter are all of those extra objects
which you would generally expect to find in a particular area. If the player
requires these things for an action, or if they look for them explicitly,
there's a chance they will find them. The player *can't* refer to these types of
things explicitly without searching for one first.

|#

;; Define the table of clutter types.

(define *clutter-table* (make-hash))

;; Rooms define the set of clutter types which apply to them.
;; Clutter types are themselves defined using ... something.


#|

The clutter concept really brings up the need for "factories" to generate
instances of entities. In addition to factories (which I assume are just
functions), we would need to be able to provide a map of values to fix in place.

|#

#|

Locations:

A location has a lot to it:

The region it's located in provides information about what "far off" might look
like.

The location has a description. The description may be altered to account for
time, weather, people, etc. Locations which are outside or adjacent to outside
especially so.

A location may contain several "fixtures" which are (in our system) things not
explicitly described in the scene but either mentioned in the description, or
just implied to be there by custom or what would be expected. This is where something like a window or the furniture might be specified. Earlier we mentioned dirt outside - this would make sense as a fixture as well. Trees, weeds, insects and more could be treated in this way.

The weather would also appear in a fixture-like way, though I don't think that belongs with the location definition.

Fixtures can be referred to explicitly by the player and they should react as
anything else might.

Zero or more clutter tables may specify the kinds of things you would expect to
find in the location. This should keep the world clean when describing things
like the contents of a desk drawer, while still allowing the player to locate a
pen and paper when necessary.

Other things we may need to consider for locations:

Exits to other places. Is this location actually an abstraction for a more
precise thing? Like being at Whiterun (from a map perspective) vs being
somewhere specific in Whiterun.

Lighting?

Faction ownership of the current land. Things that go with that like: are you allowed to be here?

Let's talk about exits...



|#

(define room1 (define/entity (@room "Room1" "Room1 Description.")))
(define room2 (define/entity (@room "Room2" "Room2 Description.")))
(define room3 (define/entity (@room "Room3" "Room3 Description.")))

(define portal "door")

(connect room1 room2 east/west #:via portal)
(connect room1 starting-room north/south)
(connect room3 room1 north/south)

(define/rule
  #:for go/before
  #:basis (action 'go _ _)
  #:rule ((action 'go dir _)
          (let* ([current-loc (ent/location)]
                 [dest-loc (current-loc . follow-towards . dir)])
            (if dest-loc
                (begin
                  (printf "Going ~a from ~a towards the ~a"
                          dir
                          (ent/name-of current-loc)
                          (ent/name-of dest-loc))
                  (ent/move-to (*current-actor*) dest-loc))
                (printf "You can't go that way.")))))

(print/room)

;; --------------------- Ok, what now?

#|

We have a notion of time. The world advances over time. There are things in the
world. And locations. We can traverse locations via compass directions. People
have a notion of anatomy. We can maim.

Our world still feels very empty. Things aren't located in a specific room. We
don't really have a lot of *things* in the first place. There are no people -
nothing actually happens.

|#

;; Entity Generation -----------------------------

#|

One of the main reasons we've built any of this is because Inform just isn't
made for dynamic worlds. The entire world is built from proper nouns.

We want dynamic worlds. With dynamic objects. To that end, we want to be able to
specify the most important traits of an object, and have the game provide the
other details.

But what might that look like?

We're defining a template for entities of a specific type. We're saying -
anything that comes out of this will be a valid ... shovel (or whatever) ... but
other than that, we haven't really said one way or another. That doesn't mean
that the generation is random. Shovels are more likely to be made out of
specific materials, and they're more likely to be specific sizes, types, and
colors. So we're defining the upper and lower bounds of shovel-ness.

There are plenty of issues with this approach, but in the interest of not
getting bogged down in the details that end with kanren, we should just build
something that mostly works most of the times.

So what does a shovel generator look like?

|#

#|

Ok, so here we have a template for constructing shovels. It's designed to mirror
the syntax for define/entity, but with the attributes defined via functions
which return a valid value.

choose-from would be a function which when called returns one
of "shovel", "trowel", or "spade". It returns a different one when called each
time, though all of these functions should take the rng seed as an optional
parameter.

Attributes can be fixed (they don't **have** to be a function).

Another important concept are the description strings which should be lazily
evaluated and memoized. If they weren't lazy, then we would have to make
stipulations about what could appear in each description string depending on the
order in which it was added to the entity.

Within description strings, each value interpolated should come from a function
which takes the entity as a parameter.

|#

;; The above template would expand into something like this:



(define *current-entity* (make-parameter player))

(define/component digging (level))

(define (choose-from #:seed [seed #f] . rst)
  (when seed (random-seed seed))
  (random-ref rst))

(define (::entity-name)
  (thing-name (component:for-entity struct:thing (*current-entity*))))

(define (::entity-desc)
  (thing-desc (component:for-entity struct:thing (*current-entity*))))

(define (lazy-string str . rst)
  (λ (ent)
    ;; For each element in rest, if it's a procedure, call it with ent. If it's
    ;; not, just use the value directly.

    ;; Actually, we should use a parameter for this. *current-ent* would refer
    ;; to the entity currently described. We could actually define several of
    ;; these parameters to basically build out a "context" for description
    ;; building.

    (parameterize ([*current-entity* ent])
      (apply format str (map (λ (v) (if (procedure? v) (v) v)) rst)))))


(define (shovel-template)
  (define/entity
    (@thing (choose-from "shovel" "trowel" "spade")
            (lazy-string "This is the description of the ~a." ::entity-name))
    (@digging 5)))

;; So in this conception, the description isn't a string, but a procedure which
;; evaluates to a string. This will require us to change everything that talks
;; about descriptions, so we want to make sure we're on the right track with
;; this.

#;(define/template shovel-template
    (@thing (choose-from "shovel" "trowel" "spade")
            (format "This is the description of the ~a." name))
    (@digging 5))

#|

TODO:

Prefabs (requires substantial changes to ECS)
Template Macro - might not be super necessary at the moment.
Nail down description generators.

Locations:

Clutter
Scenery / Fixtures

The Map:

More thought about how the world itself is put together. Is there a combat grid
scale? How is the world generated? How do we specify prefabs?

People, Factions, Relations, etc:

All of the fun social stuff! We haven't even started thinking about this sort of
thing.

|#

#|

The Map:

Ok, so I think we've decided on 3 levels of map.

World Map :: The world map represents the world from the view point of landmarks
and destinations. If there's fast travel, or route planning, or transportation
other than on foot, it would happen on this map. If there are warring factions or caravans or anything like that, they would also happen on this map.

The World Map is represented as a graph of Point-of-Interest nodes, and path
edges. The paths contain associated data regarding the length of the path (how
long it would take to traverse), perhaps the expected weather, and perceived
danger due to bandits and savagery.

I'm not 100% sure how to connect the world map locations to the other layers...

Area Map :: I guess this is the main map - the map with all the rooms that the
player traverses during normal gameplay. At this level, the player performs most
non-combat actions including dialog and general object interaction. The world at
this level is composed mostly of a regular grid of areas. Areas can connect to
other areas (for instance: Going down into a sewer, or the basement of a tavern.
Going upstairs. Climbing a tree or cliff. Etc).

TODO: Implement the idea of looking into a neighboring area, and implement a
similar one-way mechanism for looking over a cliff, or for looking out of a
second story window.

Local Map / Combat Map :: This map provides a crude tile-based representation of
an area. Each room tile is expanded out into the appropriate local map based on
a collection of prefabs or maybe a WFC algorithm in the future. From this map,
the player will largely issue orders relating to combat (though they can do
other things). The representation of other things in the world will be as simple
as possible in this scheme. The tile and the room description need to
correspond.

To do this, we probably need:

Looking at exits (and the type of exit if that's applicable) tells us what
shape the tile should be.

The type of the room will tell us about any style concerns. This might include
terrain considerations like rubble on the ground, pillars, fences, brush, trees,
etc.

Things in the room have to correspond to things in the described room. So if
there are trees, the player needs to be able to interact with them.

For rooms with furniture, the tile should specify places that furniture and
other such fixtures could be applied.

This means that we need to say where everything is located in a room? Or do we
need to just make sure that we have rules for placing those kinds of things, and
prefabs to support it.

This brings up one more consideration: Size. How do we want to handle bigger
pieces of furniture or other furnishings? I think we will want to support a good
set of "props" for fun tactical combat. Things like cover (furniture, pillars,
etc), ledges (not as dramatic as cliffs - still conceptually on the same
z-level) (you have the high ground!).

Some things may be irregularly shaped, or even shaped such that it appears to be
multiple discontiguous things (picture a statue supported by 4 pillars).





For now we need to focus on implementing the area map and the local map. These
things need to be developed in tandem. In some ways the area map is like a
summary of the local map, and in some ways it's the other way around. the local
map lacks most of the details irrelevant to combat, while the area map lacks the
spatial information necessary to conduct combat.

How are Area Maps defined? An area map is defined as having specific dimensions,
then regions are carved out (either by the generator or by hand) which will use
specific location generators. Then, fixed locations on the map are added. These
are locations with known position and content. I guess we might have things
where we know the position, but not the content (there is *something* cool here
but idk what). We could also have the other way around where we know the content
but not the position.

At this point, we know what sorts of things will be present, but locations
haven't been fully realized at this point. That will happen during gameplay as
the player moves from place to place I think?

A downside of the laziness is that we wont be able to simulate things until you
visit them (or adjacent to them or whatever). This might not be what we want..


Our first area is going to be a wooded forest area with a small village. I guess
conceptually this area *is* the village and surrounding woods.

For now, the area map will be a 2d grid (probably a hashmap of hashmaps). We
will set all of the grid spaces to belong to the temperate-forest archetype.

For now there will be no actual town, and no points of interest. Just a grid of
generated forest locations.

How can we make sure that every location is connected?? Since it doesn't matter
that much, we can just iterate over every tile and select exits at random. If
the neighboring tile doesn't have the corresponding exit, then we just add it.

If there were points of interest, then we would add them last, they would force
neighboring tiles to accommodate them, and I think we would be done with that
part.

For sure we will have to revisit this though - there is so much more to it, but
I guess one of the benefits we will see here is that we can have entirely
different generators for different types of areas.

|#

(define (forest-archetype-template)
  (define/entity
    (@room "Forest Zone" "This is the description of the forest zone.")
    #;(@thing (choose-from "shovel" "trowel" "spade")
            (lazy-string "This is the description of the ~a." ::entity-name))
    #;(@digging 5)))

(define (generate-forest dimensions)
  (let ([area-map (make-hash)])
    (for* ([x (in-range 0 dimensions)]
           [y (in-range 0 dimensions)])
      (printf "~nGenerating: ~a, ~a" x y)

      (unless (hash-has-key? area-map x)
        (hash-set! area-map x (make-hash)))

      ;; Create the area.
      (let* ([area (forest-archetype-template)]
             [exits (random-sample '(north south east west)
                                    (random 1 4)
                                    #:replacement? #f)])

        ;; TODO: Populate the area?

        (for ([e (in-list exits)])
          (let-values ([(x y) (case e
                                ['north (values x (- y 1))]
                                ['south (values x (+ y 1))]
                                ['east  (values (+ x 1) y)]
                                ['west  (values (- x 1) y)])])
            ;; If the adjacent room has been created, then connect them.
            (when (hash-ref (hash-ref area-map x (make-hash)) y #f)
              (connect area
                       (hash-ref (hash-ref area-map x) y)
                       (case e
                         [(north south) north/south]
                         [(east west) east/west])))))

        ;; Add to the map.
        (hash-set! (hash-ref area-map x) y area)))
    area-map))

#|

Ok, so we've got this notion of an area map.

Now we want to try and tackle the local map. For any particular area, we can
lookup a local map to represent that area. In the future we may be able to
generate these on the fly, but for this first approach we will try and use a
simpler LUT/prefab approach.

The main things we care about are the archetype of the area, and the
configuration of exits. Through these things, we can try and find a tile to
match, or use a fallback tile in the event that no match can be found.

Ok, that's easy enough, but now we need to talk about how things are laid out
spatially. Specifically, when the player enters the area, where are they
standing??

Where we put things within an area (in terms of local map coordinates) depends
on how we're putting them there. If someone / something is dropping them, then
we find an available space relative to where they're located.

If they're being spawned there, we will need to ask for coordinates, or a
placement specification.

A placement specification is a description of where something should go - it
could be coordinates, it could be something like "along a wall", "next to a
doorway", "on top of a surface". It could be referring to a specific type of
tagged spot. Tagged spots are how we would convey things like: It would make a
lot of sense to put furniture here. or This would be a good spot for a treasure
chest.

Tagged spots could alternatively be associated with a template generator. If we
associate templates and tagged locations and specific furniture items to a set
of common tags, then we could match up specific instances of furniture with
spots, and fill in empty spots based on generators registered for that type.
This is not too different from a loot list.

The game isn't meant to be played on the local map - leaving an area during
combat (generally through the exits) has the same effect as fleeing from combat.

Not everything in the world is represented on the local map.

We only need to keep a reference to a single local map at a time.

|#


(struct LocalMapTile (solid opaque tags) #:mutable)

(define (grid-set! g x y v)
  (unless (hash-ref g x #f)
    (hash-set! g x (make-hash)))
  (hash-set! (hash-ref g x (make-hash)) y v))

(define (grid-get g x y [v #f])
  (hash-ref (hash-ref g x (make-hash)) y v))

(define *current-local-map* (make-parameter #f))

;; Given a map m (a hash table of hash tables), and a direction d, make all of
;; the tiles on the corresponding side opaque.
(define (build-wall m d)
  (let* ([x-dim (hash-count m)]
         [y-dim (hash-count (hash-ref m (car (hash-keys m))))]
         [ranges (case d
                   ['north (cons 0 (in-range x-dim))]
                   ['south (cons (- y-dim 1) (in-range x-dim))]
                   ['east  (cons (- x-dim 1) (in-range y-dim))]
                   ['west  (cons 0 (in-range y-dim))])])
    (for ([a (cdr ranges)])
      (let ([x (if (member d '(north south)) a (car ranges))]
            [y (if (member d '(east west)) a (car ranges))])
        (set-LocalMapTile-solid! (grid-get m x y) #t)
        (set-LocalMapTile-opaque! (grid-get m x y) #t)))))

(define (build-exit m d)
  (let* ([x-dim (hash-count m)]
         [y-dim (hash-count (hash-ref m (car (hash-keys m))))]
         [x-mid (+ (quotient x-dim 2) 0)]
         [y-mid (+ (quotient y-dim 2) 0)])
    (let-values ([(x y) (case d
                          ['north (values x-mid 0)]
                          ['south (values x-mid (- x-dim 1))]
                          ['east  (values (- x-dim 1) y-mid)]
                          ['west  (values 0 y-mid)])])
      (let ([tile (grid-get m x y)])
        (set-LocalMapTile-solid! tile #f)
        (set-LocalMapTile-opaque! tile #f)

        ;; Add the exit tag to this location.
        (unless (member 'exit (LocalMapTile-tags tile))
          (set-LocalMapTile-tags! tile (cons 'exit (LocalMapTile-tags tile))))))))

(define (get-local-map area)
  ;; TODO: Idk what to even put here...
  (let ([local-map (make-hash)])
    (for* ([x (in-range 31)]
           [y (in-range 31)])
        (grid-set! local-map x y (LocalMapTile #f #f '())))

    ;; Annotate Exits and Build Walls on Non-Exit sides.
    (for ([d (in-list '(north south east west))])
      (build-wall local-map d))
    (for ([e (room/exits area)])
      (build-exit local-map e))

    local-map))

(define (print-map m)

  ;; TODO: Draw the player, other actors, furniture / containers, etc.

  (for ([y (in-range (hash-count m))])
    (for ([x (in-range (hash-count m))])
      (printf "~a"
              (cond
                [(LocalMapTile-solid (grid-get m x y)) "# "]
                [else                                  ". "])))
    (printf "~n")))

(define forest-map (generate-forest 5))
(ent/move-to player (grid-get forest-map 2 2))
(print/room)
(define test-map (get-local-map (grid-get forest-map 2 2)))


#|

What do the individual tiles of the local map look like? They're defined mostly
by their position, but they also need to be able to track metadata about what
sorts of things might make sense there. That's used during spawning to make sure
things end up where it makes sense.

|#



;; ------------------- Scopes and Queries

#|

Ok, this is long past due. Time to figure out how we can talk about things.


|#


;; Given an entity, return everything reachable by that entity. Reachable here
;; refers to everything in the same location as the entity in question.
;; Reachable includes things on surfaces, and in open containers, but excludes
;; things in closed containers.
(define (reachable [ent (*current-actor*)])
  (let* ([loc (ent/location ent)]
         ;; Add things in the same room
         [things (get-contained-by loc)]
         ;; Add things on surfaces in the room.
         [things (append things
                         (apply append (racket/base/map get-supported-by things)))]
         ;; Add contents of containers in room.
         ;; TODO: Exclude inventories (containers on actor entities).
         [things (append things
                         (apply append (racket/base/map get-contained-by things)))]
         ;; Make sure the entity itself is not listed.
         [things (filter-not (λ (e) (eq? (entity-eid ent) (entity-eid e))) things)]
         ;; TODO: Add contents of inventory of this entity if the entity has an inventory.
         )
    things))

(define (reachable? [ent (*current-entity*)]
                    #:by [by (*current-actor*)])
  (member ent (reachable by)))

;; This is basically like the threading macro.
(define (.. ent . ops)
  (foldl (λ (op acc) (op acc)) ent ops))

;; Now that we've got a way to query for reachable things, we can go back and
;; rework some of our more basic commands. Essentially by using the #:basis
;; reachable? we can stipulate whether the rule applies.
;; Taking things


;; Only take things you can reach.

(define/rule
  #:for take/before
  #:basis (action 'take (not (? reachable?)) #f)
  #:rule ((action _ obj _)
          ;; TODO: This message should be different depending on whether the
          ;; entity can see the object and whether they "know about" the object.
          (printf "~nYou can't reach the ~a from here.~n" (ent/name-of obj))
          (fail)))

;; Reaching inside of a container to take something.
(define/rule
  #:for take/before
  ;; TODO: The entity being taken needs to be inside of the thing being taken
  ;; from.
  #:basis (action 'take _ (? reachable))
  #:rule ((action _ obj cont)
          (unless (or (cont . contains . obj)
                      (cont . supports . obj)
                      (cont . traverse-by-contains . obj)
                      (cont . traverse-by-supports . obj))
            (printf "~nYou aren't able to find that in here.~n")
            (fail))))

(define (gib thing [ent (*current-actor*)])
  (thing . not-supported-by . 'anything)
  (thing . not-contained-by . 'anything)
  (thing . contained-by . ent))

(define (༼つ◕_◕༽つ thing [ent (*current-actor*)])
  (gib thing ent))

(define/rule
  #:for take/carry-out
  #:rule ((action _ obj _) (gib obj)))

(define/rule
  #:for take/report
  #:rule ((action _ obj _)
          (printf "~nTaken.~n~n")))

