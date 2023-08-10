#pragma once
#include "vendor/DenseHashMap/dense_hash_map.hpp"
#include "vendor/StaticTypeInfo/type_index.h"

#include <vector>
#include <memory>
#include <tuple>
#include <functional>
#include <limits>

namespace fdn
{
	/*! @brief Invalid entity alias. */
	#define NULL_ENTITY (std::numeric_limits<uint32_t>::max() << 12)

	/*! @brief Opaque entity type. */
	using entity = uint32_t;

	using namespace static_type_info;

	/*! @brief Top most base of sparse set, containing the sparse set array and dense set array. */
	struct SparseSetBase
	{
		SparseSetBase() noexcept = default;
		virtual ~SparseSetBase() noexcept = default;

		std::vector<entity> SparseArray;
		std::vector<entity> DenseArray;

		virtual void Release(const uint32_t entityIdentity) noexcept = 0;
	};

	/*! @brief Templated derived sparse set containing the actual component of the template type. */
	template<typename ComponentType>
	struct Pool : public SparseSetBase
	{
		Pool() noexcept = default;
		virtual ~Pool() noexcept override final = default;

		std::vector<ComponentType> Components;

		virtual void Release(const uint32_t entityIdentity) noexcept override final
		{
			if constexpr (!std::is_empty_v<ComponentType>)
			{
				std::swap(Components.back(), Components[entityIdentity]);
				Components.pop_back();
			}
		}
	};

	/*! @brief Pool alias for sake of brevity. */
	using ComponentPool = std::unique_ptr<SparseSetBase>;

	/*! @brief Dense map alias for sake of brevity. */
	template<typename KEY, typename VALUE>
	using DenseMap = jg::dense_hash_map<KEY, VALUE>;

	/*! @brief TypeIndex alias for sake of brevity. */
	using TypeIndex = static_type_info::TypeIndex;

	template<typename>
	struct examine_invocable;
	template<typename R, typename... Args>
	struct examine_invocable < std::function<R(Args...)>> {
		using type = R(Args...);
	};

	template<typename F>
	using examine_invocable_t = typename examine_invocable<F>::type;

	template<typename... ComponentTypes>
	class Collection
	{
	public:
		explicit Collection(const std::tuple<Pool<ComponentTypes>&...>& pools) noexcept
			: m_ComponentPools{ pools },
			m_EntityIdentifiers{ &std::get<0>(pools).DenseArray }
		{
			std::apply([&](auto&... pool)
				{
					((m_EntityIdentifiers = (pool.DenseArray.size() < m_EntityIdentifiers->size()) ? &pool.DenseArray : m_EntityIdentifiers), ...);
				}, m_ComponentPools);
		}

		//Currently duplicates work, this should be handled in the future!!
		template<typename Function>
		void Do(Function&& invocable) const noexcept
		{
			using F = decltype(std::function{ invocable });

			[&] <typename R, typename... Args>(R(*)(Args...)) {
				if constexpr (sizeof...(Args) == 1 && (std::is_same_v<Args, entity> && ...))
				{
					for (int entityIndex = static_cast<int>(m_EntityIdentifiers->size()) - 1; entityIndex >= 0; --entityIndex)
					{
						const entity e = (*m_EntityIdentifiers)[entityIndex];
						const uint32_t entityIdentifier = e >> 12;
						if (HasAllOf(entityIdentifier))
						{
							std::invoke(invocable, e);
						}
					}
				}
				else
				{
					static_assert(!(std::is_empty_v<std::remove_reference_t<Args>> || ...) && "Empty types are not iterated.");

					for (int entityIndex = static_cast<int>(m_EntityIdentifiers->size()) - 1; entityIndex >= 0; --entityIndex)
					{
						const entity e = (*m_EntityIdentifiers)[entityIndex];
						const uint32_t entityIdentifier = e >> 12;

						if (HasAllOf(entityIdentifier))
						{
							std::apply(invocable, std::forward_as_tuple(Get<std::remove_reference_t<Args>>(e) ...));
						}
					}
				}
			}((examine_invocable_t<F> *) nullptr);
		}

	private:
		template<typename T>
			requires std::is_same_v<entity, T>
		inline [[nodiscard]] entity Get(const entity e) const noexcept
		{
			return e;
		}

		template<typename T>
			requires !std::is_same_v<entity, T>
		inline [[nodiscard]] T& Get(const entity e) const noexcept
		{
			auto& pool = std::get<Pool<std::remove_reference_t<T>>&>(m_ComponentPools);
			const uint32_t entityIdentifier = e >> 12;
			return pool.Components[pool.SparseArray[entityIdentifier]];
		}

		inline [[nodiscard]] bool HasAllOf(const uint32_t entityIdentity) const noexcept
		{
			return (Has<ComponentTypes>(entityIdentity, std::get<Pool<ComponentTypes>&>(m_ComponentPools)) && ...);
		}

		template<typename ComponentType>
		inline [[nodiscard]] bool Has(const uint32_t entityIdentity, const Pool<ComponentType>& pool) const noexcept
		{
			return entityIdentity < pool.SparseArray.size() && pool.SparseArray[entityIdentity] != NULL_ENTITY;
		}

	private:
		std::tuple<Pool<ComponentTypes>&...> m_ComponentPools;
		std::vector<uint32_t>* m_EntityIdentifiers;
	};

	template<typename ComponentType>
	class Collection<ComponentType>
	{
	public:
		explicit Collection(Pool<ComponentType>& pool) noexcept
			: m_Pool{ pool }
		{}

		void Do(std::invocable<ComponentType&> auto&& invocable) const noexcept
		{
			static_assert(!std::is_empty_v<ComponentType> && "Empty types are not iterated.");

			for (int i{ static_cast<int>(m_Pool.DenseArray.size()) - 1 }; i >= 0; --i)
				std::invoke(invocable, m_Pool.Components[i]);
		}

		void Do(std::invocable<entity, ComponentType&> auto&& invocable) const noexcept
		{
			static_assert(!std::is_empty_v<ComponentType> && "Empty types are not iterated.");

			for (int i{ static_cast<int>(m_Pool.DenseArray.size()) - 1 }; i >= 0; --i)
				std::invoke(invocable, m_Pool.DenseArray[i], m_Pool.Components[i]);
		}

		void Do(std::invocable<entity> auto&& invocable) const noexcept
		{
			for (int i{ static_cast<int>(m_Pool.DenseArray.size()) - 1 }; i >= 0; --i)
				std::invoke(invocable, m_Pool.DenseArray[i]);
		}

		void Do(std::invocable<void> auto&& invocable) const noexcept
		{
			invocable();
		}

	private:
		Pool<ComponentType>& m_Pool;
	};

	class EntityManager
	{
	public:
		/**
		 * @brief Constructs and sets up the Entity manager.
		 * @param size_t An optional initial entity capacity.
		 */
		explicit EntityManager(const size_t capacity = 10'000) noexcept
		{
			m_Entities.reserve(capacity);
			m_Available = 0u;
			m_Next = 0u;
		}

		/**
		 * @brief Checks whether any entities are still alive.
		 * @return The truth condition of whether any entities are alive.
		 */
		[[nodiscard]] bool AnyEntitiesExist() const noexcept
		{
			return (!(m_Available == m_Entities.size()));
		}

		/**
		 * @brief Returns the total number of alive entities.
		 * @return The number of entities alive.
		 */
		[[nodiscard]] size_t GetEntityAliveCount() const noexcept
		{
			return (m_Entities.size() - m_Available);
		}

		/**
		 * @brief Masks out and returns the version from the entity id.
		 * @param entity The complete entity id with both identity and version.
		 * @return The masked out version.
		 */
		[[nodiscard]] uint32_t GetVersion(const entity entityID) const noexcept
		{
			constexpr const uint32_t VERSION_MASK = 0b00000000000000000000111111111111;
			return entityID & VERSION_MASK;
		}

		/**
		 * @brief Masks out and returns the identity from the entity id.
		 * @param entity The complete entity id with both identity and version.
		 * @return The masked out identity.
		 */
		[[nodiscard]] inline uint32_t GetIdentity(const entity entityID) const noexcept
		{
			return (entityID >> 12);
		}

		/**
		 * @brief Determines the existence of a given entity.
		 * @param entity The complete entity id with both identity and version.
		 * @return The truth condition of the existence of a specific entity.
		 */
		[[nodiscard]] bool Exists(const entity entityID) const noexcept
		{
			const auto entityIdentity = GetIdentity(entityID);
			return (entityIdentity < m_Entities.size() && m_Entities[entityIdentity] == entityID);
		}

		/**
		 * @brief Creates and returns a new entity or, if possible, recycles a previously destroyed entity.
		 * @return The created entity.
		 */
		entity CreateEntity() noexcept
		{
			return m_Available > 0 ? RecycleEntity() : CreateNewEntity();
		}

		/**
		 * @brief Destroys the entity and all of its associated components.
		 * @param entity The entity to destroy.
		 */
		void DestroyEntity(const entity entityID)
		{
			const uint32_t entityToDestroyIdentity = GetIdentity(entityID);

			//Remove all components:
			for (auto& [typeIndex, pool] : m_Pools)
			{
				if (pool && entityToDestroyIdentity < pool->SparseArray.size() && pool->SparseArray[entityToDestroyIdentity] != NULL_ENTITY)
				{
					const auto last = GetIdentity(pool->DenseArray.back());
					const auto denseAndComponentArrayIndex = pool->SparseArray[entityToDestroyIdentity];
					std::swap(pool->DenseArray.back(), pool->DenseArray[denseAndComponentArrayIndex]);
					pool->Release(denseAndComponentArrayIndex);
					std::swap(pool->SparseArray[last], pool->SparseArray[entityToDestroyIdentity]);
					pool->DenseArray.pop_back();
					pool->SparseArray[entityToDestroyIdentity] = NULL_ENTITY;
				}
			}

			const entity meldedEntity = (m_Next << 12) | (GetVersion(entityID) + 1);

			m_Entities[entityToDestroyIdentity] = meldedEntity;
			m_Next = entityToDestroyIdentity;

			m_Available++;
		}

		template<typename ComponentType>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		inline void ValidatePool() noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			if (!m_Pools[ID].get()) [[unlikely]]
			{
				m_Pools[ID] = std::make_unique<Pool<ComponentType>>();
				m_Pools[ID]->SparseArray.resize(1, NULL_ENTITY);
			}
		}

		template<typename ComponentType>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		inline size_t GetEntityCountForPool() noexcept
		{
			ValidatePool<ComponentType>();

			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();
			return m_Pools[ID]->DenseArray.size();
		}

		/**
		 * @brief Creates and adds a new component of type ComponentType with arguments Args for entity.
		 * @param entity The entity ID to add component for.
		 * @tparam Args The arguments to construct the component object with.
		 * @return A reference to the newly created component.
		 */
		template<typename ComponentType, typename... Args>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (!std::is_empty_v<ComponentType>)
			ComponentType& Add(const entity entityID, Args&&... args) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			Pool<ComponentType>* pPool = static_cast<Pool<ComponentType>*>(m_Pools[ID].get());
			if (!pPool) [[unlikely]]
			{
				m_Pools[ID] = std::make_unique<Pool<ComponentType>>();
				pPool = static_cast<Pool<ComponentType>*>(m_Pools[ID].get());
				pPool->SparseArray.resize(1, NULL_ENTITY);
			}
			Pool<ComponentType>& pool = *pPool;

			const uint32_t sparseSetPosition = static_cast<uint32_t>(pool.DenseArray.size());
			const uint32_t entityIdentity = GetIdentity(entityID);

			if (pool.SparseArray.size() <= entityIdentity)
				pool.SparseArray.resize(entityIdentity * 2, NULL_ENTITY);

			pool.SparseArray[entityIdentity] = sparseSetPosition;
			pool.DenseArray.emplace_back(entityID);
			pool.Components.emplace_back(std::forward<Args>(args)...);

			return pool.Components.back();
		}

		template<typename ComponentType, typename... Args>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& std::is_empty_v<ComponentType> && (sizeof...(Args) == 0)
			void Add(const entity entityID, Args&&... args) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			Pool<ComponentType>* pPool = static_cast<Pool<ComponentType>*>(m_Pools[ID].get());
			if (!pPool) [[unlikely]]
			{
				m_Pools[ID] = std::make_unique<Pool<ComponentType>>();
				pPool = static_cast<Pool<ComponentType>*>(m_Pools[ID].get());
				pPool->SparseArray.resize(1, NULL_ENTITY);
			}
			Pool<ComponentType>& pool = *pPool;

			const uint32_t sparseSetPosition = static_cast<uint32_t>(pool.DenseArray.size());
			const uint32_t entityIdentity = GetIdentity(entityID);

			if (pool.SparseArray.size() <= entityIdentity)
				pool.SparseArray.resize(entityIdentity * 2, NULL_ENTITY);

			pool.SparseArray[entityIdentity] = sparseSetPosition;
			pool.DenseArray.emplace_back(entityID);
		}

		template<typename ComponentType, typename... Args>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (!std::is_empty_v<ComponentType>)
			ComponentType& Replace(const entity entityID, Args&&... args) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			Pool<ComponentType>& pool = static_cast<Pool<ComponentType>&>(*m_Pools[ID].get());
			const uint32_t entityIdentity = GetIdentity(entityID);

			const uint32_t componentIndex = pool.SparseArray[entityIdentity];
			pool.Components[componentIndex] = ComponentType(std::forward<Args>(args)...);

			return pool.Components[componentIndex];
		}

		template<typename ComponentType, typename... Args>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (!std::is_empty_v<ComponentType>)
			ComponentType& AddOrReplace(const entity entityID, Args&&... args) noexcept
		{
			if (Has<ComponentType>(entityID))
				return Replace<ComponentType>(entityID, std::forward<Args>(args) ...);
			else
				return Add<ComponentType>(entityID, std::forward<Args>(args) ...);
		}

		template<typename ComponentType, typename... Args>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& std::is_empty_v<ComponentType> && (sizeof...(Args) == 0)
			void AddOrReplace(const entity entityID, Args&&... args) noexcept
		{
			if (!Has<ComponentType>(entityID))
				Add<ComponentType>(entityID);
		}


		template<typename ComponentType, typename... ComponentTypes>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (sizeof...(ComponentTypes) == 0)
			void Remove(const entity entityID) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();
			Pool<ComponentType>& pool = static_cast<Pool<ComponentType>&>(*m_Pools[ID].get());

			const uint32_t entityIdentity = GetIdentity(entityID);

			const auto lastEntityIdentifier = pool.DenseArray.back() >> 12;
			const auto denseAndComponentArrayIndex = pool.SparseArray[entityIdentity];
			std::swap(pool.DenseArray.back(), pool.DenseArray[denseAndComponentArrayIndex]);
			if constexpr (!std::is_empty_v<ComponentType>)
			{
				std::swap(pool.Components.back(), pool.Components[denseAndComponentArrayIndex]);
				pool.Components.pop_back();
			}
			std::swap(pool.SparseArray[lastEntityIdentifier], pool.SparseArray[entityIdentity]);
			pool.DenseArray.pop_back();
			pool.SparseArray[entityIdentity] = NULL_ENTITY;
		}

		template<typename ComponentType, typename... ComponentTypes>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (sizeof...(ComponentTypes) > 0)
			void Remove(const entity entityID) noexcept
		{
			Remove<ComponentType>(entityID);
			Remove<ComponentTypes...>(entityID);
		}

		/**
		 * @brief Checks whether an entity has a certain component type.
		 * @param entity The entity ID to verify component type existence for.
		 * @return A truth condition (bool) depending on component existence.
		 */
		template<typename ComponentType>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		[[nodiscard]] bool Has(const entity entityID) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			const uint32_t entityIdentity = GetIdentity(entityID);
			const Pool<ComponentType>* pool = static_cast<Pool<ComponentType>*>(m_Pools[ID].get());
			const std::vector<entity>& sparseArray = pool->SparseArray;

			return (pool && (entityIdentity < sparseArray.size()) && (sparseArray[entityIdentity] != NULL_ENTITY));
		}

		template<typename... ComponentTypes>
			requires (std::is_same_v<ComponentTypes, std::decay_t<ComponentTypes>> && ...)
		[[nodiscard]] bool HasAllOf(const entity entityID) noexcept
		{
			return (Has<ComponentTypes>(entityID) && ...);
		}

		template<typename... ComponentTypes>
			requires (std::is_same_v<ComponentTypes, std::decay_t<ComponentTypes>> && ...)
		[[nodiscard]] bool HasAnyOf(const entity entityID) noexcept
		{
			return (Has<ComponentTypes>(entityID) || ...);
		}

		/**
		 * @brief Returns a single component of type ComponentType for the entity.
		 * @param entity The entity ID to return the component reference for.
		 * @return A reference to the component belonging to entity.
		 */
		template<typename ComponentType, typename... C1>
			requires (sizeof...(C1) == 0) && std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (!std::is_empty_v<ComponentType>)
			[[nodiscard]] ComponentType& Get(const entity entityID) noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			Pool<ComponentType>& pool = static_cast<Pool<ComponentType>&>(*m_Pools[ID].get());
			return pool.Components[pool.SparseArray[GetIdentity(entityID)]];
		}

		/**
		 * @brief Returns multiple components for the entity.
		 * @param entity The entity ID to return the component references for.
		 * @return A tuple containing the component references for all requested components belonging to the entity.
		 */
		template<typename ComponentType, typename... ComponentTypes>
			requires (sizeof... (ComponentTypes) > 0) && std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		&& (std::is_same_v<ComponentTypes, std::decay_t<ComponentTypes>> && ...)
			&& (!std::is_empty_v<ComponentType>) && (!std::is_empty_v<ComponentTypes> && ...)
			[[nodiscard]] std::tuple<ComponentType&, ComponentTypes&...> Get(const entity entityID) noexcept
		{
			return std::forward_as_tuple(Get<ComponentType>(entityID), (Get<ComponentTypes>(entityID)) ...);
		}

		/**
		 * @brief Completely resets the EntityManager and all associated data.
		 */
		void Reset() noexcept
		{
			m_Entities.clear();
			m_Next = 0u;
			m_Available = 0u;

			m_Pools.clear();
		}

		template<typename... ComponentTypes>
			requires (std::is_same_v<ComponentTypes, std::decay_t<ComponentTypes>> && ...)
		&& (sizeof...(ComponentTypes) > 1)
			[[nodiscard]] Collection<ComponentTypes...> Collect() noexcept
		{
			return Collection<ComponentTypes...>(std::forward_as_tuple(GetPool<ComponentTypes>() ...));
		}

		template<typename ComponentType>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		[[nodiscard]] Collection<ComponentType> Collect() noexcept
		{
			return Collection<ComponentType>(GetPool<ComponentType>());
		}

	private:
		template<typename ComponentType>
			requires std::is_same_v<ComponentType, std::decay_t<ComponentType>>
		[[nodiscard]] Pool<ComponentType>& GetPool() noexcept
		{
			static constexpr TypeIndex ID = getTypeIndex<ComponentType>();

			ValidatePool<ComponentType>();

			return static_cast<Pool<ComponentType>&>(*m_Pools[ID].get());
		}

		/**
		 * @brief Recycles a previously destroyed entity. Always prioritized over creating a completely new entity.
		 * @return A recycled entity, corresponding to the latest destroyed entity.
		*/
		[[nodiscard]] entity RecycleEntity() noexcept
		{
			const uint32_t nextEntity = m_Entities[m_Next];
			const uint32_t entityVersion = GetVersion(nextEntity);
			const entity completeEntityID = ((m_Next << 12) | entityVersion);

			const uint32_t nextEntityIdentity = GetIdentity(nextEntity);
			m_Entities[m_Next] = completeEntityID;
			m_Next = nextEntityIdentity;

			m_Available--;
			return completeEntityID;
		}

		/**
		 * @brief Creates a completely new entity.
		 * @return The newly created entity.
		*/
		[[nodiscard]] entity CreateNewEntity() noexcept
		{
			const uint32_t entityIdentity = m_Next++;
			const entity completeEntityID = entityIdentity << 12;
			m_Entities.emplace_back(completeEntityID);
			return completeEntityID;
		}

	private:
		/*! @brief All entities, both alive and destroyed. */
		std::vector<entity> m_Entities;
		/*! @brief Number of entities that have been destroyed but not recycled. */
		std::size_t m_Available;
		/*! @brief The entity identity next in turn to be recycled. */
		entity m_Next;
		/*! @brief The dense hash map indexed by a compile time type hash id. It contains all sparse sets (component pools).*/
		DenseMap<TypeIndex, ComponentPool> m_Pools;
	};
}